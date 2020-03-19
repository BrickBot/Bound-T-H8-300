-- Programs.Execution.Tables (body)
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: programs-execution-tables.adb,v $
-- Revision 1.8  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.7  2009-01-18 07:58:32  niklas
-- Removed unused context clauses and locals.
--
-- Revision 1.6  2008/11/09 21:43:05  niklas
-- BT-CH-0158: Output.Image (Time_T) replaces Programs.Execution.Image.
--
-- Revision 1.5  2005/10/09 08:10:23  niklas
-- BT-CH-0013.
--
-- Revision 1.4  2005/09/23 10:51:41  niklas
-- BT-CH-0012.
--
-- Revision 1.3  2005/08/24 10:17:04  niklas
-- Using the new inquiry function Programs.Node (Call).
--
-- Revision 1.2  2005/02/16 21:11:47  niklas
-- BT-CH-0002.
--
-- Revision 1.1  2004/05/01 09:56:09  niklas
-- First version.
--


package body Programs.Execution.Tables is


   use type Processor.Time_T;


   function Is_Null (Item : Time_Range_T) return Boolean
   is
   begin

      return Item.Min > Item.Max;

   end Is_Null;


   --
   --    Helpful operations
   --


   procedure Add_Number (
      Number : in     Natural;
      To     : in out Natural)
   --
   -- Adds the Number To a sum.
   --
   is
   begin

      To := To + Number;

   end Add_Number;


   procedure Add_Time (
      Time : in     Processor.Time_T;
      To   : in out Processor.Time_T)
   --
   -- Adds the Number To a sum.
   --
   is
   begin

      To := To + Time;

   end Add_Time;


   procedure Include (
      Time   : in     Processor.Time_T;
      Within : in out Time_Range_T)
   --
   -- Includes a Time Within a time-range, possibly widening the range.
   --
   is
   begin

      Within.Min := Processor.Time_T'Min (Within.Min, Time);
      Within.Max := Processor.Time_T'Max (Within.Max, Time);

   end Include;


   function Union (Left, Right : Time_Range_T) return Time_Range_T
   --
   -- Union of time intervals.
   --
   is
   begin

      if    Is_Null (Left ) then return Right;
      elsif Is_Null (Right) then return Left;
      else

         return (
            Min => Processor.Time_T'Min (Left.Min, Right.Min),
            Max => Processor.Time_T'Max (Left.Max, Right.Max));

      end if;

   end Union;


   --
   --    Adding a link to a share
   --


   procedure Add (Share : in Share_T; To : in out Share_T)
   --
   -- Adds the given Share To a sum of shares.
   --
   is
   begin

      To.Link_Paths     := To.Link_Paths     + Share.Link_Paths;
      To.Counted_Paths  := To.Counted_Paths  + Share.Counted_Paths;
      To.Calls          := To.Calls          + Share.Calls;
      To.Timed_Paths    := To.Timed_Paths    + Share.Timed_Paths;
      To.Asserted_Paths := To.Asserted_Paths + Share.Asserted_Paths;

      To.Time           := Union (To.Time         , Share.Time         );
      To.Time_Per_Call  := Union (To.Time_Per_Call, Share.Time_Per_Call);

      To.Bounded_Paths  := To.Bounded_Paths  + Share.Bounded_Paths;
      To.Bounded_Calls  := To.Bounded_Calls  + Share.Bounded_Calls;
      To.Total_Time     := To.Total_Time     + Share.Total_Time;

   end Add;


   procedure Add_To_Share(
      Paths         : in     Natural;
      Bounds        : in     Bounds_Ref;
      Count_Bounded : in     Boolean;
      Count         : in     Flow.Execution.Count_T;
      Share         : in out Share_T)
   --
   -- Adds the given Bounds to the Share, knowing that these Bounds
   -- are reached via Paths new link-paths (that is, link-paths not
   -- before considered in this Share). Count_Bounded shows whether the
   -- execution count of each of these paths is bounded, and Count is
   -- then the bound on the total execution count of all these paths.
   --
   -- This operation shall not be used in cases where only some of
   -- the Paths have a bounded execution count -- it should be all or
   -- none, as shown by Count_Bounded.
   --
   is

      Full_Time : Processor.Time_T;
      -- The bound on the execution time in Bounds, if any.
      -- The time spent in lower-level callees is included.

   begin

      Add_Number (Number => Paths, To => Share.Link_Paths);

      if Count_Bounded then

         Add_Number (Number => Paths, To => Share.Counted_Paths);

         Add_Number (Number => Count, To => Share.Calls);

      end if;

      if Time_Bounded (Bounds) then

         Add_Number (Number => Paths, To => Share.Timed_Paths);

         if Time_Asserted (Bounds) then

            Add_Number (Number => Paths, To => Share.Asserted_Paths);

         end if;

         Full_Time := Time (Bounds);

         Include (Time => Full_Time, Within => Share.Time);

         if (not Count_Bounded) or else Count > 0 then
            -- These Bounds are, or might be, on the worst-case path.

            Include (Time => Full_Time, Within => Share.Time_Per_Call);

         end if;

         if Count_Bounded then

            Add_Number (Number => Paths, To => Share.Bounded_Paths);

            Add_Number (Number => Count, To => Share.Bounded_Calls);

            Add_Time (Time => Count * Full_Time, To => Share.Total_Time);

         end if;

      end if;

   end Add_To_Share;


   procedure Add_To_Self (
      Bounds : in     Bounds_Ref;
      Factor : in     Flow.Execution.Count_T;
      Calls  : in     Call_Bounds_List_T;
      Self   : in out Processor.Time_T)
   --
   -- Divides the total execution time of the Bounds (taken Factor
   -- times) into a "self" part and a "non-self" or "callee" part that
   -- is spent in the Calls from the Bounds. Adds the "self" part to
   -- the Self parameter.
   --
   -- This assumes that the Bounds have a bounded execution time and
   -- bounded execution counts; otherwise the operation is null.
   --
   is

      Total    : Processor.Time_T;
      Non_Self : Processor.Time_T;
      -- The total and the "non-self" time.

   begin

      if Time_Bounded (Bounds) then
         -- There is a time bound, either asserted or computed.

         Total := Time (Bounds);

         if Time_Asserted (Bounds) then
            -- An asserted time is "all self".

            Add_Time (
               Time => Factor * Total,
               To   => Self);

         elsif Counts_Set (Bounds) then
            -- We should be able to divide the Total_Time into a
            -- Self time and a Non_Self (callee) time.

            Non_Self := Total_Time (Calls, Bounds);

            if Non_Self > Total then

               Output.Fault (
                  Location => "Programs.Execution.Tables.Add_To_Self",
                  Locus    => Locus (Bounds),
                  Text     =>
                       "Non_Self time ="
                     & Output.Image (Non_Self)
                     & " exceeds Total time ="
                     & Output.Image (Total));

            else

               Add_Time (
                  Time => Factor * (Total - Non_Self),
                  To   => Self);

            end if;

         end if;

      end if;

   end Add_To_Self;


   --
   --    Table indices
   --


   subtype Possible_Row_Index_T is Natural;
   --
   -- The index of a row in a table, or zero to mean "no row".


   No_Row : constant Possible_Row_Index_T := 0;
   --
   -- To indicate that no table row has been assigned.


   subtype Row_Index_T is Possible_Row_Index_T
      range 1 .. Possible_Row_Index_T'Last;
   --
   -- The index of a row in a table.



   --
   --    Collecting execution bounds from a root closure
   --


   generic

      with procedure Visit (
         Bounds         : in     Bounds_Ref;
         Factor_Bounded : in     Boolean;
         Factor         : in     Flow.Execution.Count_T;
         Calls          : in     Call_Bounds_List_T;
         Go_Below       :    out Boolean);
      --
      -- Visits the given Bounds, knowing that these Bounds are invoked
      -- from one new (not before considered) link-path a total of Factor
      -- times in higher-level (caller) bounds (if Factor_Bounded).
      -- Sets Go_Below to show whether Calls, the links from these
      -- Bounds to callees, should also be traversed.


   procedure Bounds_Traversal (Root : in Bounds_Ref);
   --
   -- Traverses all (or perhaps not) link-paths in the Root bounds,
   -- calling Visit once for each link-path to a given Bounds,
   -- collecting execution-count information on the way. The visit
   -- order is top-down, depth-first.


   procedure Bounds_Traversal (Root : in Bounds_Ref)
   is

      procedure Visit_Subtree (
         Bounds         : in Bounds_Ref;
         Factor_Bounded : in Boolean;
         Factor         : in Flow.Execution.Count_T)
      --
      -- Visits the given Bounds, and lower-level callee bounds if
      -- permitted, knowing that these Bounds are invoked from one new
      -- (not before considered) link-path a total of Factor times in
      -- higher-level (caller) bounds (if Factor_Bounded).
      --
      is

         Calls : constant Call_Bounds_List_T := Call_Bounds (Bounds);
         -- The bounds on feasible lower-level callees.

         Bounded_Counts : constant Boolean := Counts_Set (Bounds);
         -- Whether the execution counts of nodes (in particular,
         -- calls) in Bounds are bounded.

         Go_Below : Boolean;
         -- Whether we should visit the Calls.

         Call_Factor : Flow.Execution.Count_T := 1;
         -- The number of times one of the Calls is executed for one
         -- execution of the Bounds, if bounded, else 1.

      begin

         Visit (
            Bounds         => Bounds,
            Factor_Bounded => Factor_Bounded,
            Factor         => Factor,
            Calls          => Calls,
            Go_Below       => Go_Below);

         if Go_Below then

            for C in Calls'Range loop

               if Bounded_Counts then

                  Call_Factor := Call_Count (Calls(C).Call, Bounds);

               end if;

               Visit_Subtree (
                  Bounds         => Calls(C).Bounds,
                  Factor_Bounded => Factor_Bounded and Bounded_Counts,
                  Factor         => Factor * Call_Factor);

            end loop;

         end if;

      end Visit_Subtree;


   begin  -- Bounds_Traversal

      Visit_Subtree (
         Bounds         => Root,
         Factor_Bounded => True,
         Factor         => 1);

   end Bounds_Traversal;


   --
   --    Tables of Subprogram_Row_T
   --


   function Subprogram_Table (Root : Bounds_Ref)
   return Subprogram_Table_T
   is

      Program : constant Program_T := Programs.Execution.Program (Root);
      -- The program under analysis.

      Max_Sub_Index : constant Subprogram_Count_T :=
         Number_Of_Subprograms (Program);
      -- The total number of subprograms in the program.

      Max_Rows : constant Natural := Natural (Max_Sub_Index);
      -- The total number of subprograms in the program is an upper
      -- limit on the number of rows in the table.

      Num_Rows : Natural := 0;
      -- The number of rows defined in the table so far.

      Table : Subprogram_Table_T (1 .. Max_Rows);
      -- The table, with Table(1 .. Num_Rows) filled.

      Row_Index : array (1 .. Max_Sub_Index) of Possible_Row_Index_T := (
         others => No_Row);
      -- The row index assigned to a particular subprogram.
      -- Initially no rows have been assigned.


      procedure Add_To_Table (
         Bounds         : in     Bounds_Ref;
         Factor_Bounded : in     Boolean;
         Factor         : in     Flow.Execution.Count_T;
         Calls          : in     Call_Bounds_List_T;
         Go_Below       :    out Boolean)
      --
      -- Adds the given Bounds to the Table, knowing that these Bounds
      -- are invoked from one new (not before considered) link-path a
      -- total of Factor times in higher-level (caller) bounds (if
      -- Factor_Bounded).
      --
      -- To be used as a Visit for Bounds_Traversal. Sets Go_Below to True.
      --
      is

         Sub_Index : constant Subprogram_Index_T :=
            Index (Subprogram (Bounds));
         -- The index of the subprogram to which the Bounds apply.

         Row : Row_Index_T;
         -- The Row for Subprogram (Bounds).

      begin

         -- Find the Table Row for this subprogram:

         if Row_Index(Sub_Index) = No_Row then
            -- First encounter with this Subprogram.
            -- Allocate the next row for it:

            Num_Rows := Num_Rows + 1;

            Row := Num_Rows;

            Row_Index(Sub_Index) := Row;

            Table(Row) := (
               Subprogram => Subprogram (Bounds),
               Share      => No_Share,
               Self_Time  => 0);

         else
            -- This Subprogram already has a row allocated.

            Row := Row_Index(Sub_Index);

         end if;

         -- Add the Bounds to the Row:

         Add_To_Share (
            Paths         => 1,
            Bounds        => Bounds,
            Count_Bounded => Factor_Bounded,
            Count         => Factor,
            Share         => Table(Row).Share);

         if Factor_Bounded then

            Add_To_Self (
               Bounds => Bounds,
               Factor => Factor,
               Calls  => Calls,
               Self   => Table(Row).Self_Time);

         end if;

         Go_Below := True;

      end Add_To_Table;


      procedure Build_Table
      is new Bounds_Traversal (Visit => Add_To_Table);


   begin  -- Subprogram_Table

      Build_Table (Root);

     return Table(1 .. Num_Rows);

   exception

   when X : others =>

      Output.Exception_Info (
         Locus      => Locus (Root),
         Text       => "Programs.Execution.Tables.Subprogram_Table",
         Occurrence => X);

      return Table(1 .. 0);

   end Subprogram_Table;


   --
   --   Tables of Call_Row_T
   --


   function Call_Table (Root : Bounds_Ref)
   return Call_Table_T
   is

      Max_Sub_Index : constant Subprogram_Count_T :=
         Number_Of_Subprograms (Program (Root));
      -- The total number of subprograms in the program under analysis.

      Max_Calls : constant Natural :=
         Number_Of_All_Calls (Program (Root));
      -- The total number of calls (call sites, Call_T) in the program
      -- under analysis.

      Table : Call_Table_T (1 .. Max_Calls);
      -- Working space for tabulating the calls.
      -- At first, each subprogram is given a slice of this Table with
      -- as many elements as there are call sites (Call_T) in the
      -- subprogram. Then, some (or all) of these elements are used to
      -- hold the Call_Rows with this subprogram as the Callee.
      -- The unused elements are marked by Caller = No_Subprogram.
      -- Finally, the unused elements are deleted and the dense
      -- Table is returned.

      First_Free_Row : Row_Index_T := 1;
      -- The first free (unallocated) row in the Table.

      Last_Row : Natural := 0;
      -- The final result will be Table(1 .. Last_Row), after omitting
      -- the unused Table elements.

      type Ref_T is record
         First_Row : Possible_Row_Index_T;
         Last_Row  : Possible_Row_Index_T;
      end record;
      --
      -- For each subprogram we process (in the Root closure), pointers
      -- to the Table-slice that holds the Call_Rows with this
      -- subprogram as the Callee.
      --
      -- First_Row
      --    The first row in Table allocated to calls from this
      --    subprogram. If this is No_Row, the Ref_T is unused (it
      --    corresponds to a Subprogram_Index that does not occur
      --    in the closure of this Root).
      -- Last_Row
      --    The last row in Table containing calls from this
      --    subprogram. If < First_Row, there are none.
      --
      -- The range Table(First_Row .. Last_Row) contains the so-far
      -- defined Call_Time_Rows with this subprogram as the Callee.

      No_Ref : constant Ref_T := (
         First_Row => No_Row,
         Last_Row  => No_Row);
      -- A nice initial value.

      Ref : array (1 .. Max_Sub_Index) of Ref_T := (others => No_Ref);
      -- Reference information for all subprogram indices.


      procedure Find_Row (
         Call : in     Call_T;
         Ref  : in out Ref_T;
         Row  :    out Row_Index_T)
      --
      -- Finds or creates the Table row for this Call.
      -- The Ref parameter is the Ref entry for Caller(Call), possibly
      -- still No_Ref.
      --
      is

         Callee : constant Subprogram_T := Programs.Callee (Call);
         -- The callee.

         Poss_Row : Possible_Row_Index_T := No_Row;
         -- A possible existing row.

      begin

         -- Have we seen this caller subprogram before?

         if Ref.First_Row = No_Row then
            -- Well hello, new caller subprogram! We will give
            -- you some space in the Table, right away!

            Ref.First_Row := First_Free_Row;
            Ref.Last_Row  := First_Free_Row - 1;

            First_Free_Row :=
                 First_Free_Row
               + Number_Of_Calls_From (Caller (Call));

         end if;

         -- Find the Table row for this caller-callee combination:

         for R in Ref.First_Row .. Ref.Last_Row loop

            if Table(R).Callee = Callee then
               -- Found it.

               Poss_Row := R;

               exit;

            end if;

         end loop;

         if Poss_Row /= No_Row then
            -- The row already exists.

            Row := Poss_Row;

         else
            -- This caller-callee combination is a new one.
            -- Take a new Table element for it.

            Ref.Last_Row := Ref.Last_Row + 1;

            Row := Ref.Last_Row;

            Table(Row) := (
               Caller => Caller (Call),
               Callee => Callee,
               Share  => No_Share);

         end if;

      end Find_Row;


      procedure Add_To_Table (
         Bounds         : in     Bounds_Ref;
         Factor_Bounded : in     Boolean;
         Factor         : in     Flow.Execution.Count_T;
         Calls          : in     Call_Bounds_List_T;
         Go_Below       :    out Boolean)
      --
      -- Adds the Calls from the given Bounds to the Table, knowing
      -- (if Factor_Bounded is True) that these Bounds are invoked in
      -- total Factor times in higher-level (caller) bounds.
      --
      -- This operation is presented as a Visit for Bounds_Traversal,
      -- so the Go_Below parameter is set True to include callees.
      --
      is

         Caller : constant Subprogram_Index_T := Index (Subprogram (Bounds));
         -- The index of the bounded subprogram; the caller.

         Count_Bounded : constant Boolean :=
            Factor_Bounded and Counts_Set (Bounds);
         -- Whether the total execution count of a call is bounded.

         Call_Factor : Flow.Execution.Count_T := 1;
         -- The number of times one of the Calls is executed per
         -- execution of the caller, if known, else 1.

         Call_Row : Row_Index_T;
         -- The table row for a call.

      begin

         for C in Calls'Range loop

            if Count_Bounded then

               Call_Factor := Call_Count (Calls(C).Call, Bounds);

            end if;

            Find_Row (
               Call => Calls(C).Call,
               Ref  => Ref(Caller),
               Row  => Call_Row);

            Add_To_Share (
               Paths         => 1,
               Bounds        => Calls(C).Bounds,
               Count_Bounded => Count_Bounded,
               Count         => Factor * Call_Factor,
               Share         => Table(Call_Row).Share);

         end loop;

         Go_Below := True;

      end Add_To_Table;


      procedure Build_Table
      is new Bounds_Traversal (Visit => Add_To_Table);


   begin  -- Call_Table

      -- Cumulate the bounds and counts per caller-callee pair:

      Build_Table (Root);

      -- Compress the Table by omitting the unused elements:

      for T in 1 .. First_Free_Row - 1 loop

         if Table(T).Caller /= No_Subprogram then

            Last_Row := Last_Row + 1;

            Table(Last_Row) := Table(T);

         end if;

      end loop;

      return Table(1 .. Last_Row);

   exception

   when X : others =>

      Output.Exception_Info (
         Locus      => Locus (Root),
         Text       => "Programs.Execution.Tables.Call_Table",
         Occurrence => X);

      return Table(1 .. 0);

   end Call_Table;


   procedure Tabulate_Subprogram_Bounds (
      Root      : in Bounds_Ref;
      Qualified : in Boolean)
   is

      Table : constant Subprogram_Table_T := Subprogram_Table (Root);
      -- The table of time bounds per subprogram.

      Root_Nest_Mark : Output.Nest_Mark_T;
      -- Marks the Root locus in the output locus stack.

      Sep : constant Character := Output.Field_Separator;
      -- Abbreviation.

      Row : Subprogram_Row_T;
      -- The current row in the Table.

      Sub_Locus : Output.Locus_T;
      -- The locus of the subprogram of the current Row.

   begin

      Root_Nest_Mark := Output.Nest (Locus (Root));

      for T in Table'Range loop

         if Table(T).Share.Calls > 0 then
            -- This subprogram is on the worst-case path.

            Row := Table(T);

            Sub_Locus := Locus (
               Subprogram => Row.Subprogram,
               Qualified  => Qualified);

            Output.Result (
               Key  => "Time_Table",
               Text =>
                    Output.Image (Row.Share.Total_Time)
                  & Sep
                  & Output.Image (Row.Self_Time)
                  & Sep
                  & Output.Image (Row.Share.Calls)
                  & Sep
                  & Output.Image (Row.Share.Time_Per_Call.Min)
                  & Sep
                  & Output.Image (Row.Share.Time_Per_Call.Max)
                  & Sep
                  & Output.Call_Path (Sub_Locus)
                  & Sep
                  & Output.Source_File (Sub_Locus)
                  & Sep
                  & Output.Image (Output.Statements (Sub_Locus)));

         end if;

      end loop;

      Output.Unnest (Root_Nest_Mark);

   exception

   when X : others =>

      Output.Exception_Info (
         Text       => "Programs.Execution.Tables.Tabulate_Time_Bounds",
         Occurrence => X);

      Output.Unnest (Root_Nest_Mark);

   end Tabulate_Subprogram_Bounds;


   --
   --    Tables of subprograms and their bounds
   --


   function Subprograms_Under (
      Root   : Bounds_Ref;
      Within : Bounds_Set_T)
   return Subprogram_List_T
   is

      Bounds_Seen :  Bounds_Subset_T (Max_Size => Number_Of_Bounds (Within));
      -- The set of execution bounds visited so far.
      -- Initially empty by default.

      Subs_Seen : Subprogram_Set_T;
      -- The set of subprograms visited so far.
      -- Initially empty by default.


      procedure Visit (Bounds : in Bounds_Ref);
      --
      -- See below.


      procedure Visit_Calls (Call_Bounds : in Call_Bounds_List_T)
      --
      -- Visits the execution bounds in the Call_Bounds.
      --
      is
      begin

         for C in Call_Bounds'Range loop

            Visit (Call_Bounds(C).Bounds);

         end loop;

      end Visit_Calls;


      procedure Visit (Bounds : in Bounds_Ref)
      --
      -- Visits these Bounds and the linked bounds.
      --
      is
      begin

         if not Is_Member (Item => Bounds, Of_Set => Bounds_Seen) then
            -- We have not visited these Bounds before.

            Add (Item => Bounds, To => Bounds_Seen);

            Add (To => Subs_Seen, Adding => Subprogram (Bounds));

            Visit_Calls (Call_Bounds (Bounds));

         end if;

      end Visit;


   begin  -- Subprograms_Under

      Visit (Root);

      declare

         Subs : constant Subprogram_List_T := To_List (Subs_Seen);

      begin

         Erase (Subs_Seen);

         return Subs;

      end;

   end Subprograms_Under;


   function All_Bounds_For (
      Subprogram : Subprogram_T;
      Under_Root : Bounds_Ref;
      Within     : Bounds_Set_T)
   return Bounds_Table_T
   is

      Max_Bounds : constant Natural :=
         Number_Of_Bounds (Sub => Subprogram, Within => Within);
      -- An upper limit on the number of execution-bounds for this
      -- Subprogram under this root.

      subtype Place_T is Positive range 1 .. Max_Bounds;

      Table : Bounds_Table_T (Place_T);
      Last : Natural := 0;
      -- The bounds for the Subprogram will be collected in Table(1 .. Last).


      procedure Visit (
         Bounds         : in     Bounds_Ref;
         Factor_Bounded : in     Boolean;
         Factor         : in     Flow.Execution.Count_T;
         Calls          : in     Call_Bounds_List_T;
         Go_Below       :    out Boolean)
      --
      -- Visits these Bounds, counted Factor times (if Factor_Bounded).
      -- Sets Go_Below to True if the Calls should be traversed (that is,
      -- if these Bounds are not for the chosen Subprogram).
      --
      -- To be used as a Visit for Bounds_Traversal.
      --
      is

         New_Bounds : Boolean := True;
         -- Whether this is the first time these Bounds for the
         -- Subprogram are visited.

         Place : Place_T;
         -- The place for these Bounds in the Table.

      begin

         if Programs.Execution.Subprogram (Bounds) = Subprogram then
            -- These bounds to be included in the result.

            -- Find the Place:

            for L in 1 .. Last loop

               if Table(L).Bounds = Bounds then
                  -- These Bounds are already listed.

                  Place := L;

                  New_Bounds := False;

                  exit;

               end if;

            end loop;

            if New_Bounds then
               -- First time we visit these Bounds.
               -- Add them to the Table:

               Last := Last + 1;

               Place := Last;

               Table(Place) := (
                  Bounds    => Bounds,
                  Share     => No_Share,
                  Self_Time => 0);

            end if;

            Add_To_Share (
               Paths         => 1,
               Bounds        => Bounds,
               Count_Bounded => Factor_Bounded,
               Count         => Factor,
               Share         => Table(Place).Share);

            if Factor_Bounded then

               Add_To_Self (
                  Bounds => Bounds,
                  Factor => Factor,
                  Calls  => Calls,
                  Self   => Table(Place).Self_Time);

            end if;

            -- There is no need to visit the callees, because they
            -- cannot (in the absence of recursion) include bounds
            -- of this Subprogram:

            Go_Below := False;

         else
            -- Need to visit the callees to find instances of
            -- this Subprogram.

            Go_Below := True;

         end if;

      end Visit;


      procedure Build_Table
      is new Bounds_Traversal (Visit => Visit);


   begin  -- All_Bounds_For

      Build_Table (Root => Under_Root);

      return Table(1 .. Last);

   end All_Bounds_For;


   function Min_Time (Bounds : Bounds_Table_T)
   return Bounds_Row_T
   is

      Quickest : Bounds_Row_T := No_Bounds_Row;
      -- The minimum.

      Candidate : Bounds_Ref;
      -- One of the Bounds.

   begin

      for B in Bounds'Range loop

         Candidate := Bounds(B).Bounds;

         if Time_Bounded (Candidate)
         and then (
            Quickest.Bounds = No_Bounds
            or else Time (Candidate) < Time (Quickest.Bounds))
         then
            -- A quicker one.

            Quickest := Bounds(B);

         end if;

       end loop;

      return Quickest;

   end Min_Time;


   function Max_Time (Bounds : Bounds_Table_T)
   return Bounds_Row_T
   is

      Slowest : Bounds_Row_T := No_Bounds_Row;
      -- The maximum.

      Candidate : Bounds_Ref;
      -- One of the Bounds.

   begin

      for B in Bounds'Range loop

         Candidate := Bounds(B).Bounds;

         if Time_Bounded (Candidate)
         and then (
            Slowest.Bounds = No_Bounds
            or else Time (Candidate) > Time (Slowest.Bounds))
         then
            -- A slower one.

            Slowest := Bounds(B);

         end if;

       end loop;

      return Slowest;

   end Max_Time;


   function Is_Null (Item : Count_Range_T) return Boolean
   is
   begin

      return Item.Min > Item.Max;

   end Is_Null;


   procedure Include (
      Count  : in     Flow.Execution.Count_T;
      Within : in out Count_Range_T)
   --
   -- Includes the Count Within the range, extending the range as needed.
   --
   is
   begin

      Within.Min := Flow.Execution.Count_T'Min (Within.Min, Count);
      Within.Max := Flow.Execution.Count_T'Max (Within.Max, Count);

   end Include;


   function Total_Times_And_Counts (
      Executing : Subprogram_T;
      Bounds    : Bounds_Table_T)
   return Total_Times_And_Counts_T
   is

      Graph : constant Flow.Graph_T := Flow_Graph (Executing);
      -- The flow-graph of the subprogram.

      Totals : Total_Times_And_Counts_T (
         Nodes => Flow.Max_Node (Graph),
         Edges => Flow.Max_Edge (Graph));
      -- The totals to be cumulated.

      Candidate : Bounds_Ref;
      -- One of the candidate terms from Bounds.

      Candidate_Share : Share_T;
      -- The execution-share of the candidate.

      Candidate_Times_Bounded : Boolean;
      -- Whether the execution time of nodes and edges is
      -- bounded, in the Candidate.

      Candidate_Counts_Bounded : Boolean;
      -- Whether the execution count of nodes and edges is
      -- bounded, in the Candidate.

      Candidate_Calls_Bounded : Boolean;
      -- Whether some number of calls of the Candidate is known.

      Candidate_Fully_Bounded : Boolean;
      -- Whether the Candidate is bounded enough to compute its
      -- contribution to the total execution time.


      procedure Add_Candidate (
         Time  : in     Processor.Time_T;
         Self  : in     Processor.Time_T;
         Count : in     Flow.Execution.Count_T;
         To    : in out Total_Time_Count_T)
      --
      -- Adds the Time etc for a given node or edge in the Candidate,
      -- To the Totals summary for that node or edge, depending on which
      -- aspects of the Candidate are bounded.
      --
      is

         Bounded_Count : Flow.Execution.Count_T;
         -- The total number of times the node or edge is executed
         -- in a path to the Candidate where the number of calls
         -- is bounded and the execution-counts and execution-time
         -- of the Candidate is bounded.

      begin

         -- Times per call:

         if Candidate_Times_Bounded then

            Include (Time => Time, Within => To.Time);

            Include (Time => Self, Within => To.Self);

            Include (Time => Time - Self, Within => To.Callees);

         end if;

         -- Counts per call, and total calls:

         if Candidate_Counts_Bounded then

            Include (Count => Count, Within => To.Count);

            if Candidate_Calls_Bounded then

               Add_Number (
                  Number => Candidate_Share.Calls * Count,
                  To     => To.Total_Count);

            end if;

         end if;

         -- Total times:

         if Candidate_Fully_Bounded then

            Bounded_Count := Candidate_Share.Bounded_Calls * Count;

            Add_Time (
               Time => Bounded_Count * Time,
               To   => To.Total_Time);

            Add_Time (
               Time => Bounded_Count * Self,
               To   => To.Total_Self);

         end if;

      end Add_Candidate;


      Node : Flow.Node_T;
      -- A node in the graph.

      Edge : Flow.Edge_T;
      -- An edge in the graph.


   begin  -- Total_Times_And_Counts

      -- Initialize the Totals:

      Totals.Subprogram := Executing;
      Totals.Share      := No_Share;
      Totals.Self_Time  := 0;
      Totals.Bounded    := 0;

      for N in Totals.Node'Range loop

         Totals.Node(N) := No_Time_Count;

      end loop;

      for E in Totals.Edge'Range loop

         Totals.Edge(E) := No_Time_Count;

      end loop;

      -- Cumulate the totals:

      for B in Bounds'Range loop

         Candidate       := Bounds(B).Bounds;
         Candidate_Share := Bounds(B).Share;

         if Subprogram (Candidate) /= Executing then

            Output.Fault (
               Location => "Programs.Execution.Tables.Total_Times_And_Counts",
               Text     =>
                    "Subprogram (Bounds ("
                  & Positive'Image (B)
                  & " ) = "
                  & Name (Subprogram (Candidate))
                  & " /= "
                  & Name (Executing));

            exit;

         end if;

         -- This Candidate stands for some execution share:

         Add (Share => Candidate_Share, To => Totals.Share);

         Add_Time (Time => Bounds(B).Self_Time, To => Totals.Self_Time);

         Candidate_Times_Bounded  := Node_Times_Bounded (Candidate);
         Candidate_Counts_Bounded := Counts_Set (Candidate);
         Candidate_Calls_Bounded  := Candidate_Share.Bounded_Paths > 0;

         Candidate_Fully_Bounded :=
                Candidate_Times_Bounded
            and Candidate_Counts_Bounded
            and Candidate_Calls_Bounded;

         if (Candidate_Times_Bounded or Candidate_Counts_Bounded)
         and not Time_Asserted (Candidate)
         then

            Add_Number (Number => 1, To => Totals.Bounded);

         end if;

         -- Cumulate each node and edge:

         for N in Totals.Node'Range loop

            Node := Flow.Node_At (Index => N, Within => Graph);

            Add_Candidate (
               Time  => Time  (Node, Candidate, With_Calls => True ),
               Self  => Time  (Node, Candidate, With_Calls => False),
               Count => Count (Node, Candidate),
               To    => Totals.Node(N));

         end loop;

         for E in Totals.Edge'Range loop

            Edge := Flow.Edge_At (Index => E, Within => Graph);

            Add_Candidate (
               Time  => Time  (Edge, Candidate),
               Self  => Time  (Edge, Candidate),
               Count => Count (Edge, Candidate),
               To    => Totals.Edge(E));

         end loop;

      end loop;

      return Totals;

   end Total_Times_And_Counts;


   --
   --    Tables of execution bounds and links between bounds
   --


   function Bounds_Table (
      Root   : Bounds_Ref;
      Within : Bounds_Set_T)
   return Bounds_Table_T
   is

      Num_Bounds : constant Bounds_Count_T := Number_Of_Bounds (Within);
      -- The total number of execution bounds in this Bounds_Set; and
      -- so an upper limit on the index of the execution bounds in the
      -- table.

      Table : Bounds_Table_T (1 .. Natural (Num_Bounds));
      Last  : Possible_Row_Index_T := No_Row;
      -- The collected bounds-rows will be Table(1 .. Last).

      Row_Index : array (1 .. Num_Bounds) of Possible_Row_Index_T := (
         others => No_Row);
      -- Shows the place in Table that holds the execution bounds with
      -- a given index. Of course, No_Row indicates that the bounds are
      -- not yet in the Table.


      procedure Add_To_Table (
         Bounds         : in     Bounds_Ref;
         Factor_Bounded : in     Boolean;
         Factor         : in     Flow.Execution.Count_T;
         Calls          : in     Call_Bounds_List_T;
         Go_Below       :    out Boolean)
      --
      -- Adds the given Bounds to the Table, knowing that these Bounds
      -- are invoked from one new (not before considered) link-path a
      -- total of Factor times in higher-level (caller) bounds (if
      -- Factor_Bounded).
      --
      -- To be used as a Visit for Bounds_Traversal. Sets Go_Below to True.
      --
      is

         Bounds_Index : constant Bounds_Index_T := Index (Bounds);
         -- The unique index of these Bounds.

         Row : Row_Index_T;
         -- The Table row for the Bounds.

      begin

         -- Find the Row:

         if Row_Index(Bounds_Index) = No_Row then
            -- First encounter with these Bounds.
            -- Allocate the next row for it:

            Last := Last + 1;

            Row := Last;

            Row_Index(Bounds_Index) := Row;

            Table(Row) := (
               Bounds    => Bounds,
               Share     => No_Share,
               Self_Time => 0);

         else
            -- These Bounds already have a row allocated.

            Row := Row_Index(Bounds_Index);

         end if;

         -- Add the Bounds to the Row:

         Add_To_Share (
            Paths         => 1,
            Bounds        => Bounds,
            Count_Bounded => Factor_Bounded,
            Count         => Factor,
            Share         => Table(Row).Share);

         if Factor_Bounded then

            Add_To_Self (
               Bounds => Bounds,
               Factor => Factor,
               Calls  => Calls,
               Self   => Table(Row).Self_Time);

         end if;

         Go_Below := True;

      end Add_To_Table;


      procedure Build_Table
      is new Bounds_Traversal (Visit => Add_To_Table);


   begin  -- Bounds_Table

     Build_Table (Root);

     return Table(1 .. Last);

   exception

   when X : others =>

      Output.Exception_Info (
         Locus      => Locus (Root),
         Text       => "Programs.Execution.Tables.Bounds_Table",
         Occurrence => X);

      return Table(1 .. 0);

   end Bounds_Table;


   function Links (From : Bounds_Row_T)
   return Links_Table_T
   is

      Calls : constant Call_Bounds_List_T := Call_Bounds (From.Bounds);
      -- The calls and bounds of feasible lower-level callees.

      Call_Count_Bounded : constant Boolean :=
         From.Share.Counted_Paths > 0 and Counts_Set (From.Bounds);
      -- Whether the number of executions (calls) of the caller is
      -- bounded, and the execution counts within the caller are
      -- bounded, so that the number of executions of all Calls are
      -- bounded.

      Table : Links_Table_T (1 .. Calls'Length);
      Last  : Possible_Row_Index_T := No_Row;
      -- The collected links-rows will be Table(1 .. Last).
      -- All Table elements have the same Caller = From.
      -- All Calls with the same Call.Bounds (and thus the same callee
      -- too) are summarised in one and the same Table element.


      procedure Add_To_Table (Call : in Call_Bounds_T)
      --
      -- Adds the given Call to the Table.
      --
      is

         Row : Possible_Row_Index_T := No_Row;
         -- The Table row for the Call.

         Count : Flow.Execution.Count_T := 0;
         -- The execution count of this Call, if bounded, else zero.

      begin

         -- Find the Row:

         for T in 1 .. Last loop

            if Table(T).Callee = Call.Bounds then
               -- These Call.Bounds already have a row allocated.

               Row := T;

               exit;

            end if;

         end loop;

         if Row = No_Row then
            -- First encounter with these Call.Bounds.
            -- Allocate the next row for it:

            Last := Last + 1;

            Row := Last;

            Table(Row) := (
               Caller => From.Bounds,
               Callee => Call.Bounds,
               Share  => No_Share);

         end if;

         -- Find the number of calls:

         if Call_Count_Bounded then

            Count :=
                 From.Share.Calls
               * Call_Count (Call => Call.Call, Within => From.Bounds);

         end if;

         -- Add the Call.Bounds to the Row, taking into account the
         -- number of link-paths to the caller, From:
         -- to the caller, From:

         Add_To_Share (
            Paths         => From.Share.Link_Paths,
            Bounds        => Call.Bounds,
            Count_Bounded => Call_Count_Bounded,
            Count         => Count,
            Share         => Table(Row).Share);

      end Add_To_Table;


   begin  -- Links_Table

     for C in Calls'Range loop

        Add_To_Table (Call => Calls(C));

     end loop;

     return Table(1 .. Last);

   exception

   when X : others =>

      Output.Exception_Info (
         Locus      => Locus (From.Bounds),
         Text       => "Programs.Execution.Tables.Links_Table",
         Occurrence => X);

      return Table(1 .. 0);

   end Links;


end Programs.Execution.Tables;
