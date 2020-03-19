-- Assertions (body)
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
-- $Revision: 1.66 $
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: assertions.adb,v $
-- Revision 1.66  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.65  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.64  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.63  2009-12-21 14:59:28  niklas
-- BT-CH-0201: Role names with blanks. Option -warn [no_]role.
--
-- Revision 1.62  2009-12-17 14:05:54  niklas
-- BT-CH-0197: Assertions on instruction roles.
--
-- Revision 1.61  2009-10-07 19:26:09  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--
-- Revision 1.60  2009-03-27 13:57:12  niklas
-- BT-CH-0167: Assertion context identified by source-code markers.
--
-- Revision 1.59  2009/03/20 18:19:29  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.58  2009/01/18 07:51:05  niklas
-- Cleaned up context clauses.
--
-- Revision 1.57  2008/11/03 07:58:13  niklas
-- BT-CH-0155: Ignore assertions on absent subprograms.
--
-- Revision 1.56  2008/09/24 08:38:51  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.55  2008/09/20 12:41:49  niklas
-- BT-CH-0145: No error re too few assertion matches if graph is growing.
--
-- Revision 1.54  2008/09/19 10:34:14  niklas
-- BT-CH-0144: Assertion "populations" work again, and more.
--
-- Revision 1.53  2008/07/28 19:23:44  niklas
-- BT-CH-0140: Detect contradictory execution-count bounds.
--
-- Revision 1.52  2008/07/23 09:07:14  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.51  2008/07/14 19:16:55  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.50  2008/02/27 14:58:47  niklas
-- BT-CH-0116: Call-specific time and stack assertions.
--
-- Revision 1.49  2007/12/17 13:54:33  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.48  2007/08/03 19:16:54  niklas
-- Extended Apply (Fact, Subprogram), Apply_To_Identified_Subprograms
-- and Apply_Options to implement tracing per Opt.Trace_Sub_Options.
--
-- Revision 1.47  2007/08/02 11:14:50  niklas
-- Extended function Callees to trace the mapping of assertions
-- to the dynamic call. To help, added an Image function for an
-- Assertion_Subset_T within an Assertion_Set_T.
--
-- Revision 1.46  2007/04/24 08:16:25  niklas
-- For Opt.Trace_Map added display of the source location of each
-- mapped assertion (location in the assertion file).
--
-- Revision 1.45  2007/01/25 21:25:13  niklas
-- BT-CH-0043.
--
-- Revision 1.44  2006/11/26 22:07:23  niklas
-- BT-CH-0039.
--
-- Revision 1.43  2006/10/28 19:52:15  niklas
-- BT-CH-0031.
--
-- Revision 1.42  2006/09/29 18:02:17  niklas
-- Corrected Collect_Cell_Set to use only single-location
-- variables and warn about (ignored) multi-location ones.
--
-- Revision 1.41  2006/08/22 13:50:47  niklas
-- BT-CH-0025.
--
-- Revision 1.40  2006/05/29 14:16:18  niklas
-- Corrected Get_Assertions to raise Input_Error if some
-- assertion file was not valid.
--
-- Revision 1.39  2006/05/29 11:22:33  niklas
-- BT-CH-0023.
--
-- Revision 1.38  2006/05/28 07:09:49  niklas
-- BT-CH-0020 further: Added Apply (Fact to Subprogram) and
-- Apply_To_Identified_Subprograms, to implement the "return"
-- and "integrate" subprogram-options.
--
-- Revision 1.37  2006/05/27 21:45:43  niklas
-- BT-CH-0020.
--
-- Revision 1.36  2005/09/03 11:50:27  niklas
-- BT-CH-0006.
--
-- Revision 1.35  2005/02/23 09:05:13  niklas
-- BT-CH-0005.
--
-- Revision 1.34  2005/02/20 15:15:35  niklas
-- BT-CH-0004.
--
-- Revision 1.33  2005/02/16 21:11:37  niklas
-- BT-CH-0002.
--
-- Revision 1.32  2005/02/04 20:59:44  niklas
-- Added the procedure Apply_Options to enter subprogram options
-- from the assertion set into the Programs data structures.
-- At present this is used only for the "[no] return" option.
--
-- Revision 1.31  2004/05/01 14:57:20  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not from Arithmetic.
-- Defined Assertion_Set_T to have reference semantics. Thus, parameters
-- of this type are now always of mode "in" even if the underlying object
-- is updated.
-- Defined Assertion_Map_T objects to have default initial value No_Map.
-- Added Symbol_Table parameter to Check_Mapping. Added Display_Properties
-- to describe the properties of loops when loop-mapping fails.
-- Added support for variables that are held in different cell(s)
-- depending on the code address. Many functions of the form Xxx_Values
-- now have two variants: one taking an assertion set and returning a
-- Var_Bound_List_T, and another taking in addition a Code_Address_T and
-- returning a Cell_Bound_List_T valid at that address. If the program
-- point is implicitly known (eg. Call_Values, Subprogram_Inputs) the
-- second variant takes only the assertion set and no program point.
-- Added assertions on subprogram inputs, valid only on entry to the
-- subprogram, retrieved with the function Subprogram_Inputs (variants
-- as explained above).
-- Using the new package Loops.Cells in the implementation. This supports
-- variables that change location depending on code address.
--
-- Revision 1.30  2003/02/17 17:18:41  holsti
-- Added the ability for a loop/call assertion to apply (map) to
-- several loops/calls. The size of this loop/call population can be
-- bounded in the assertion, by a Population component in the
-- Loop_Block_T or Call_Block_T.
-- Removed the "scope list" level of the assertion-set structure. An
-- assertion-set now has a single list of subprograms blocks. The scope
-- prefix is used only at assertion-parsing time to abbreviate symbols.
-- Added a list of Global_Statements to the assertion-set structure.
-- These global loop/call assertions are mapped in each subprogram;
-- their population bounds can be set to allow no matches or to require
-- some matches.
-- Changed Mapping_Status_T to more appropriate literal identifiers.
-- Added a reference (Source_T) to the assertion-file in many assertion
-- elements (replaces the earlier Line_Number components).
-- Derived new types from all Unbounded_Vector instances so that the
-- vector operations are directly visible without qualification (in
-- consequence, some local variables named "Last" had to be renamed).
-- Changed Loop/Call_Description_T to be undiscriminated; a null list
-- of properties handles the special case.
-- Modified reporting of loop/call matches that are too few or too many.
-- Improved Identify_Loops_And_Calls to include the global bounds (and
-- global statements) even when there are no subprogram-specific
-- assertions (the scope-list structure made this hard, before).
--
-- Revision 1.29  2003/02/17 14:22:16  holsti
-- Added the loop-property "executes <code address>" as one more
-- way to identify loops in assertions.
--
-- Revision 1.28  2001/07/03 12:09:08  holsti
-- Call assertions mapped with use of callee (NC_137).
-- Possible mappings displayed for an ambiguous mapping.
--
-- Revision 1.27  2001/04/17 13:52:34  holsti
-- Processor.Code_Address_T replaces Processor.Address_T.
--
-- Revision 1.26  2001/04/10 13:29:22  ville
-- Deleted extra notes
--
-- Revision 1.25  2001/04/10 13:04:00  ville
-- Loop_Is_Labelled implemented
--
-- Revision 1.24  2001/03/21 20:32:21  holsti
-- Output with Locus values.
--
-- Revision 1.23  2001/03/10 00:29:46  holsti
-- Unused Program_T parameters removed.
-- Analysis mode uses Arithmetic.Opt.
--
-- Revision 1.22  2001/02/15 14:16:24  ville
-- Analysis mode assertions enabled
--
-- Revision 1.21  2000/12/29 14:38:04  holsti
-- Loop_Invariants added.
--
-- Revision 1.20  2000/12/29 13:20:06  holsti
-- Removed tbas etc. in favour of NCs.
--
-- Revision 1.19  2000/12/28 17:49:52  holsti
-- Execution_Time_Bound_T uses Processor.Time_T.
--
-- Revision 1.18  2000/12/28 12:57:29  holsti
-- Subprograms and cells are expected to be looked-up during parsing,
-- and found in the AST; look-up code deleted here.
-- Use Arithmetic.Bound_T instead of syntax-oriented, local Bound_T.
-- Loop- and call-mapping reimplemented, with some tracing output under
-- control of the Trace_Map option.
--
-- Revision 1.17  2000/12/05 15:40:13  holsti
-- Adapted to new name of Decoder.Default_Properties.
--
-- Revision 1.16  2000/12/05 11:40:56  holsti
-- Fixed assertion-mapping for subprogram with no assertions.
--
-- Revision 1.15  2000/11/29 19:42:21  holsti
-- Number of property-tables per assertion map corrected.
--
-- Revision 1.14  2000/11/29 15:02:39  holsti
-- Query functions simplified by a set of lower-level accessors.
-- Property map added to assertion mapping.
-- Check_Loop_And_Call_Blocks choice of mapped loop-index corrected.
-- Ambiguous mapping-status separated from Unmapped status.
-- Moved "use" clauses from context into package.
-- Duplicated descriptions of public subprograms removed.
--
-- Revision 1.13  2000/11/22 22:28:56  holsti
-- Using Processor.Code_Address_T instead of Processor.Address_T.
--
-- Revision 1.12  2000/11/22 15:04:59  langback
-- Bug fixes.
--
-- Revision 1.11  2000/11/22 12:48:33  langback
-- Intermediate version.
--
-- Revision 1.10  2000/11/06 08:23:30  langback
-- Added check for empty Assertion_Set also to the routine
-- "Identify_Loops_And_Calls".
--
-- Revision 1.9  2000/11/03 13:42:12  langback
-- Added check of assertion set (map) "emptiness" in all functions that
-- take user assertion information as parameters. Necessary after changes
-- in type declarations of the types Assertion_Set_T and Assertion_Map_T.
--
-- Revision 1.8  2000/10/31 11:44:33  langback
-- First version after adding abstract syntax tree specific child
-- package. Implemented a number of "tba"s.
--
-- Revision 1.7  2000/10/12 11:13:37  langback
-- First committed version with access functions implemented.
--
-- Revision 1.6  2000/09/07 13:07:51  langback
-- Minor fixes.
--
-- Revision 1.5  2000/09/01 08:15:22  saarinen
-- Some fixes and other minor changes.
--
-- Revision 1.4  2000/08/21 13:08:09  holsti
-- Loopless graphs allowed ('Last may be zero).
--
-- Revision 1.3  2000/08/04 15:03:35  langback
-- First version of Get added.
-- Incomplete version of Identify_Loops_And_Calls added.
-- Stubs added for a number of functions previously added to .ads file.
--
-- Revision 1.2  2000/04/21 20:01:42  holsti
-- Describe usage if error in arguments.
--


with Arithmetic.Opt;
with Assertions.Opt;
with Assertions.Own;
with Assertions.Own.Text;
with Assertions.Parser;
with Assertions.Source_Marks;
with Flow.Show;
with Loops.Show;
with Options.File_Sets;
with Options.String_Sets;
with Output;
with Processor.Properties;
with Storage.List_Cell_Sets;
with String_Pool;
with Unbounded_Vectors;


package body Assertions is


   use Assertions.Own;


   --
   ---   Assertion sets from Assertions.Own:
   --


   type Assertion_Set_Object_T is new Own.Assertion_Bag_T;


   procedure Get_Assertions (
      Program             : in     Programs.Program_T;
      Assertion_Set       :    out Assertion_Set_T)
   is

      Files : Options.String_Sets.String_List_T :=
         Options.File_Sets.To_List (Opt.Assertion_Files);
      -- All the assertion file names.

      Valid : Boolean;
      -- Whether an assertion file was good.

      All_Valid : Boolean := True;
      -- Whether all assertion files were good.

   begin

      Assertion_Set := new Assertion_Set_Object_T;

      for F in Files'Range loop

         Parser.Parse_File (
            File_Name => String_Pool.To_String (Files(F)),
            Program   => Program,
            Into      => Assertion_Bag_T (Assertion_Set.all),
            Valid     => Valid);

         All_Valid := All_Valid and Valid;

      end loop;

      if not All_Valid then

         raise Input_Error;

      end if;

   end Get_Assertions;


   procedure Get_Marks
   is

      Files : Options.String_Sets.String_List_T :=
         Options.File_Sets.To_List (Opt.Mark_Files);
      -- All the mark-definition file names.

      Valid : Boolean;
      -- Whether a mark-definition file was good.

      All_Valid : Boolean := True;
      -- Whether all mark-definition files were good.

   begin

      for F in Files'Range loop

         Source_Marks.Load_File (
            File_Name => String_Pool.To_String (Files(F)),
            Valid     => Valid);

         All_Valid := All_Valid and Valid;

      end loop;

      if not All_Valid then

         raise Input_Error;

      end if;

   end Get_Marks;


   procedure Apply (
      Fact : in Fact_T;
      To   : in Programs.Subprogram_T)
   --
   -- Applies a Fact To a subprogram.
   --
   is

      Sub_Mark : Output.Nest_Mark_T;
      -- For the subprogram locus.

   begin

      Sub_Mark := Output.Nest (Programs.Locus (To));

      if Opt.Trace_Sub_Options then

         Output.Trace ("Applying fact " & Own.Text.Image (Fact));

      end if;

      case Fact.Kind is

      when Return_Method =>

         Programs.Set_Return_Method (
            Subprogram => To,
            To         => Fact.Return_Method);

      when Subprogram_Arithmetic =>

         if Fact.Use_Arithmetic then

            Programs.Set_Arithmetic_Analysis (
               Subprogram => To,
               Choice     => Arithmetic.Opt.Enforced);

         else

            Programs.Set_Arithmetic_Analysis (
               Subprogram => To,
               Choice     => Arithmetic.Opt.Disabled);

         end if;

      when Subprogram_Integrate =>

         Programs.Set_Call_Integration (
            Subprogram => To,
            To         => True);

      when Subprogram_Hide =>

         Programs.Set_Hiding_In_Call_Graph_Drawing (
            Subprogram => To,
            To         => True);

      when Subprogram_Omit =>

         Programs.Set_Stub (
            Subprogram => To,
            To         => True);

      when Subprogram_Unused =>

         Programs.Set_Unused (
            Subprogram => To,
            To         => True);

      when Property_Value =>

         Processor.Properties.Apply (
            Property => Fact.Property,
            Values   => Fact.Prop_Value,
            To       => To);

      when others =>

         null;

      end case;

      Output.Unnest (Sub_Mark);

   exception

   when others =>

      Output.Unnest (Sub_Mark);

   end Apply;


   procedure Apply_To_Identified_Subprograms (
      Fact      : Fact_T;
      Predicate : Part_Predicate_T)
   --
   -- Applies a Fact to the subprogram or subprograms that are
   -- identified by the Predicate, which belongs to a Parts_T
   -- with Kind = Subprogram.
   --
   is

      Feature : Feature_T;
      -- A feature in the Predicate.

   begin

      for P in Predicate'Range loop

         Feature := Predicate(P);

         if Opt.Trace_Sub_Options then

            Output.Trace ("Applying feature" & Positive'Image (P));

            Own.Text.Put (Item => Feature, Indent => 6, Outer => No_Goals);

         end if;

         case Feature.Kind is

         when Subprogram_Identity =>

            if not Feature.Negated then
               -- This identifies a subprogram.

               Apply (
                  Fact => Fact,
                  To   => Feature.Subprogram);

            end if;

         when others =>

            null;  -- TBD for Predicate(P).Kind = Named?

         end case;

      end loop;

   end Apply_To_Identified_Subprograms;


   procedure Apply_To_Instruction (
      Address : in Processor.Code_Address_T;
      Fact    : in Fact_T;
      Program : in Programs.Program_T)
   --
   -- Applies a Fact to the instruction at the given Address.
   --
   is

      Conflict : Boolean;
      -- Whether the new role conflicts with a role that
      -- was assigned earlier to this instruction.

   begin

      case Fact.Kind is

      when Instruction_Role =>

         Programs.Set_Instruction_Role (
            Address  => Address,
            Role     => Fact.Role,
            Within   => Program,
            Conflict => Conflict);

         if Conflict then

            Output.Error (
               Locus => Locus (Fact.Source),
               Text  => "Conflicts with earlier assertions.");

         end if;

      when others =>

         null;

      end case;

   end Apply_To_Instruction;


   procedure Apply_Options (
      Assertion_Set : in Assertion_Set_T;
      Program       : in Programs.Program_T)
   is

      Object : Assertion_Set_Object_T renames Assertion_Set.all;

      Ass : Assertion_T;
      -- One of the assertions.

   begin

      if Opt.Trace_Sub_Options then

         Output.Trace ("Applying asserted subprogram options.");

      end if;

      for A in First (Object) .. Last (Object) loop

         Ass := Element (Object, A);

         if Opt.Trace_Sub_Options then

            Output.Trace ("Applying assertion" & Positive'Image (A));

            Own.Text.Put (Ass);

         end if;

         case Ass.Parts.Kind is

         when Subprogram =>

            case Ass.Fact.Kind is

            when Return_Method
               | Subprogram_Arithmetic
               | Subprogram_Integrate
               | Subprogram_Omit
               | Subprogram_Unused
               | Subprogram_Hide
               | Property_Value =>

               Apply_To_Identified_Subprograms (
                  Fact      => Ass.Fact,
                  Predicate => Ass.Parts.Predicate.all);

            when others =>

               null;

            end case;

         when Instruction =>

            Apply_To_Instruction (
               Address => Ass.Parts.Address,
               Fact    => Ass.Fact,
               Program => Program);

         when others =>

            null;

         end case;

      end loop;

   end Apply_Options;


   procedure Warn_About_Unused_Options (
      Assertion_Set : in Assertion_Set_T;
      Program       : in Programs.Program_T)
   is

      Object : Assertion_Set_Object_T renames Assertion_Set.all;

      Ass : Assertion_T;
      -- One of the assertions.

   begin

      -- The "options" checked now include:
      --
      -- > Instruction roles.

      for A in First (Object) .. Last (Object) loop

         Ass := Element (Object, A);

         case Ass.Parts.Kind is

         when Instruction =>

            case Ass.Fact.Kind is

            when Instruction_Role =>

               if Opt.Warn_Unused_Role
               and then not Programs.Instruction_Role_Was_Used (
                  Address => Ass.Parts.Address,
                  Within  => Program)
               then

                  Output.Warning (
                     Locus => Locus (Ass.Fact.Source),
                     Text  => "Instruction role assertion was not used.");

               end if;

            when others =>

               null;

            end case;

         when others =>

            null;

         end case;

      end loop;

   end Warn_About_Unused_Options;


   --
   ---   Choosing subsets of assertions and mapping subsets
   ---   to lists of results (transformed assertions) or
   ---   a cumulative (folded) result.
   --


   generic
      type Params (<>) is private;
   package Subsets
   --
   -- Defining subsets of assertions by means of a predicate
   -- function with a certain type of filtering Parameters.
   --
   is

      type Filter_T is access function (
         Assertion : Assertion_T;
         Param     : Params)
      return Boolean;

      function Subset (
         From   : Assertion_Bag_T;
         Filter : Filter_T;
         Param  : Params)
      return Assertion_Subset_T;
      --
      -- The subset of assertions From the set, chosen by the
      -- Filter function (returning True for "chosen") depending
      -- on the given filtering Parameters.

      function Subset (
         From   : Assertion_Subset_T;
         Within : Assertion_Bag_T;
         Filter : Filter_T;
         Param  : Params)
      return Assertion_Subset_T;
      --
      -- The sub-subset of assertions From the given subset Within
      -- the given assertion set, chosen by the Filter function
      -- (returning True for "chosen") depending on the given
      -- filtering Parameters.

   end Subsets;


   package body Subsets is

      function Subset (
         From   : Assertion_Bag_T;
         Filter : Filter_T;
         Param  : Params)
      return Assertion_Subset_T
      is

         Sub : Assertion_Subset_T (1 .. Length (From));
         Num : Natural := 0;
         -- The result will be Sub(1 .. Num).

      begin

         for F in First (From) .. Last (From) loop

            if Filter (Element (From, F), Param) then
               -- This assertion is chosen for the result.

               Num := Num + 1;

               Sub(Num) := F;

            end if;

         end loop;

         return Sub(1 .. Num);

      end Subset;

      function Subset (
         From   : Assertion_Subset_T;
         Within : Assertion_Bag_T;
         Filter : Filter_T;
         Param  : Params)
      return Assertion_Subset_T
      is

         Sub : Assertion_Subset_T (1 .. From'Length);
         Num : Natural := 0;
         -- The result will be Sub(1 .. Num).

      begin

         for F in From'Range loop

            if Filter (Element (Within, From(F)), Param) then
               -- This assertion is chosen for the result.

               Num := Num + 1;

               Sub(Num) := From(F);

            end if;

         end loop;

         return Sub(1 .. Num);

      end Subset;

   end Subsets;


   generic
      type Result      is private;
      type Result_List is array (Positive range <>) of Result;
      type Params (<>) is private;
   package Projections
   --
   -- Projecting an assertion subset to a list of computed Results
   -- for each assertion in the subset.
   --
   is

      type Projector_T is access function (
         Assertion : Assertion_T;
         Param     : Params)
      return Result;

      function Projection (
         From      : Assertion_Subset_T;
         Within    : Assertion_Bag_T;
         Projector : Projector_T;
         Param     : Params)
      return Result_List;
      --
      -- The list of the Results returned by the Projector function
      -- for the assertions From the given subset Within the given
      -- assertion set and using the given projection Parameters.

   end Projections;


   package body Projections is

      function Projection (
         From      : Assertion_Subset_T;
         Within    : Assertion_Bag_T;
         Projector : Projector_T;
         Param     : Params)
      return Result_List
      is

         Results : Result_List (From'Range);
         -- The results.

      begin

         for R in Results'Range loop

            Results(R) := Projector (
               Assertion => Element (Within, From(R)),
               Param     => Param);

         end loop;

         return Results;

      end Projection;

   end Projections;


   generic
      type Result (<>) is private;
      type Params (<>) is private;
   package Cumulations
   --
   -- Accumulating a total Result from a subset of assertions
   -- with the help of some Parameters.
   --
   is

      type Cumulator_T is access procedure (
         Assertion : in     Assertion_T;
         Param     : in     Params;
         Total     : in out Result);

      procedure Cumulate (
         From      : in     Assertion_Subset_T;
         Within    : in     Assertion_Bag_T;
         Cumulator : in     Cumulator_T;
         Param     : in     Params;
         Total     : in out Result);
      --
      -- Accumulates a Total result by applying the Cumulator
      -- to each assertion From the subset Within the whole set,
      -- using the same Parameters for all calls.

   end Cumulations;


   package body Cumulations is

      procedure Cumulate (
         From      : in     Assertion_Subset_T;
         Within    : in     Assertion_Bag_T;
         Cumulator : in     Cumulator_T;
         Param     : in     Params;
         Total     : in out Result)
      is
      begin

         for F in From'Range loop

            Cumulator (
               Assertion => Element (Within, From(F)),
               Param     => Param,
               Total     => Total);

         end loop;

      end Cumulate;

   end Cumulations;


   --
   ---   Subsetting utilities
   --


   -- Part and facts


   type Part_Facts_T is record
      Part  : Part_Kind_T;
      Facts : Fact_Kinds_T;
   end record;
   --
   -- Params for selecting an assertion subset based on the
   -- kind of part and fact. One kind of part but many kinds
   -- of facts can be selected.


   package Part_Fact_Subsets is new Subsets (Params => Part_Facts_T);
   --
   -- Choosing assertion subsets based on the kind of Part and Fact.


   function Match_Part_Fact (
      Assertion : Assertion_T;
      Param     : Part_Facts_T)
   return Boolean
   --
   -- Whether the Assertion concerns a given kind of part and
   -- states a fact of some chosen kinds.
   --
   is
   begin

      return Assertion.Parts.Kind = Param.Part
         and Param.Facts(Assertion.Fact.Kind);

   end Match_Part_Fact;


   -- Subprogram, part and fact


   type Subprogram_Facts_Span_T is record
      Subprogram : Programs.Subprogram_T;
      Model      : Flow.Computation.Model_Handle_T;
      Facts      : Fact_Kinds_T;
      Span       : Fact_Span_T;
   end record;
   --
   -- Params for selecting an assertion subset based on the subject
   -- subprogram and the kind of fact asserted. For Variable_Value
   -- facts, the Span is also significant.


   package Subprogram_Facts_Span_Subsets
   is new Subsets (Params => Subprogram_Facts_Span_T);
   --
   -- Choosing assertion subsets that apply to a given subprogram
   -- and state a given kind of fact, with a given span.


   function Match_Subprogram_Fact_Span (
      Assertion : Assertion_T;
      Param     : Subprogram_Facts_Span_T)
   return Boolean
   --
   -- Whether the Assertion applies to a given Subprogram and
   -- states a given kind of fact with a given span.
   --
   is
   begin

      return (Assertion.Parts.Kind = Subprogram
         and  Param.Facts(Assertion.Fact.Kind)
         and (Assertion.Fact.Kind /= Variable_Value
              or else
              Assertion.Fact.Span = Param.Span))
         and then
             Match (
                Subprogram => Param.Subprogram,
                Model      => Param.Model,
                Predicate  => Assertion.Parts.Predicate,
                Goals      => No_Goals);

   end Match_Subprogram_Fact_Span;


   -- Part and property


   type Part_Property_T is record
      Part     : Part_Kind_T;
      Property : Processor.Property_T;
   end record;
   --
   -- Params for selecting a subset of property-value assertions
   -- based on the kind of part and on which property.


   package Part_Property_Subsets
   is new Subsets (Params => Part_Property_T);
   --
   -- Choosing assertion subsets that constrain the values of
   -- a particular processor-property in a particular part of
   -- the program.


   function Match_Part_Property (
      Assertion : Assertion_T;
      Param     : Part_Property_T)
   return Boolean
   --
   -- Whether the Assertion constrains the values of a particular
   -- processor property over a particular part of the program.
   --
   is
      use type Processor.Property_T;
   begin

      return (Assertion.Parts.Kind = Param.Part
         and  Assertion.Fact.Kind  = Property_Value)
         and then
              Assertion.Fact.Property = Param.Property;

   end Match_Part_Property;


   -- Subprogram and property


   type Subprogram_Property_T is record
      Subprogram : Programs.Subprogram_T;
      Model      : Flow.Computation.Model_Handle_T;
      Property   : Processor.Property_T;
   end record;
   --
   -- Params for selecting a subset of property-value assertions
   -- based on the subprogram and property to which the assertion
   -- applies.


   package Subprogram_Property_Subsets
   is new Subsets (Params => Subprogram_Property_T);
   --
   -- Choosing assertion subsets that constrain the values of
   -- a particular processor-property in a particular subprogram.


   function Match_Subprogram_Property (
      Assertion : Assertion_T;
      Param     : Subprogram_Property_T)
   return Boolean
   --
   -- Whether the Assertion constrains the values of a particular
   -- processor property over a particular subprogram.
   --
   is
      use type Processor.Property_T;
      use type Programs.Subprogram_T;
   begin

      return (Assertion.Parts.Kind = Subprogram
         and  Assertion.Fact.Kind  = Property_Value)
         and then
              Assertion.Fact.Property = Param.Property
         and then
              Match (
                 Subprogram => Param.Subprogram,
                 Model      => Param.Model,
                 Predicate  => Assertion.Parts.Predicate,
                 Goals      => No_Goals);

   end Match_Subprogram_Property;


   -- Instruction (address) and facts


   type Instruction_Facts_T is record
      Address : Processor.Code_Address_T;
      Facts   : Fact_Kinds_T;
   end record;
   --
   -- Params for selecting an assertion subset that asserts
   -- some kinds of Facts on the instruction at a given Address.


   package Instruction_Fact_Subsets is new
      Subsets (Params => Instruction_Facts_T);
   --
   -- Choosing assertion subsets based on an instruction address
   -- and the kinds of Facts that are asserted for the instruction.


   function Match_Instruction_Fact (
      Assertion : Assertion_T;
      Param     : Instruction_Facts_T)
   return Boolean
   --
   -- Whether the Assertion concerns an instruction at a given
   -- address and states a fact of some chosen kinds.
   --
   is
      use type Processor.Code_Address_T;
   begin

      return Assertion.Parts.Kind = Instruction
         and then Assertion.Parts.Address = Param.Address
         and then Param.Facts(Assertion.Fact.Kind);

   end Match_Instruction_Fact;


   -- Actual part


   package Actual_Part_Subsets is new Subsets (Params => Actual_Part_T);
   --
   -- Choosing assertion subsets that apply to a particular actual
   -- part of the program under analysis.


   function Match_Actual_Part (
      Assertion : Assertion_T;
      Param     : Actual_Part_T)
   return Boolean
   --
   -- Whether the Assertion applies to the actual part that is
   -- the Param. The Assertion Kind must equal the part Kind and
   -- the part must satisfy the Assertion predicate.
   --
   is
   begin

      if Assertion.Parts.Kind /= Param.Kind then
         -- Evidently no match.

         return False;

      else

         if Opt.Trace_Matching then

            Output.Trace (
                 "Matching "
               & Image (Param)
               & " to assertion:");

            Text.Put (Assertion);

         end if;

         return Match (
            Part      => Param,
            Predicate => Assertion.Parts.Predicate,
            Goals     => No_Goals);

      end if;

   end Match_Actual_Part;


   --
   ---   Projection utilities
   --


   type Null_T is null record;
   --
   -- In case parameters are not needed for a subset or projection.


   package Var_Interval_Projections is new Projections (
      Result      => Storage.Bounds.Var_Interval_T,
      Result_List => Storage.Bounds.Var_Interval_List_T,
      Params      => Null_T);
   --
   -- Projecting assertion subsets to variable-value bounds.


   function To_Var_Interval (
      Assertion : Assertion_T;
      Param     : Null_T)
   return Storage.Bounds.Var_Interval_T
   --
   -- The Var_Interval_T that corresponds to an Assertion
   -- on a Variable_Value fact.
   --
   -- Precondition: Assertion.Fact.Kind = Variable_Value.
   --
   is
   begin

      return (
         Location => Assertion.Fact.Var_Name.Location,
         Interval => Assertion.Fact.Var_Value);

   end To_Var_Interval;


   --
   ---   Cumulation utilities
   --


   -- Sets of invariant cells


   subtype Cell_Set_T is Storage.List_Cell_Sets.Set_T;
   --
   -- A set of cells that is invariant in some context.

   use Storage.List_Cell_Sets;


   package Invariant_Cell_Sets is new Cumulations (
      Result => Cell_Set_T,
      Params => Null_T);
   --
   -- Accumulating sets of cells from invariant-cell assertions.


   procedure Collect_Cell_Set (
      Assertion : in     Assertion_T;
      Param     : in     Null_T;
      Total     : in out Cell_Set_T)
   --
   -- Adds the cells from an invariance Assertion to a Total.
   --
   -- Precondition: Assertion.Fact.Kind = Variable_Invariance.
   --
   is

      Loc : Storage.Location_T renames
         Assertion.Fact.Invariant_Var_Name.Location.all;
      -- The locations of the invariant variable.

   begin

      if Loc'Length <= 1 then

         for L in Loc'Range loop

            Add (
               Cell => Loc(L).Cell,
               To   => Total);

         end loop;

      else
         -- TBD/TBM to make invariance depend on execution point.

         Output.Warning (
              "Ignored multi-location invariance assertion on "
            & Image (Assertion.Fact.Invariant_Var_Name));

      end if;

   end Collect_Cell_Set;


   -- Execution-time bounds


   package Execution_Time_Bounds is new Cumulations (
      Result => Time_Bound_T,
      Params => Null_T);
   --
   -- Accumulating the conjunctive (intersected, strictest)
   -- execution-time bounds.


   procedure Restrict_Time (
      Assertion : in     Assertion_T;
      Param     : in     Null_T;
      Total     : in out Time_Bound_T)
   --
   -- Restricts the Total execution-time bounds by the bounds from
   -- an execution-time Assertion.
   --
   -- Precondition: Assertion.Fact.Kind = Execution_Time.
   --
   is

      Time : constant Time_Bound_T := Assertion.Fact.Time;

   begin

      Total.Min := Processor.Time_T'Max (Total.Min, Time.Min);

      Total.Max := Processor.Time_T'Min (Total.Max, Time.Max);

   end Restrict_Time;


   -- Stack usage/final-height bounds:


   package Stack_Bounds is new Cumulations (
      Result => Stack_Bounds_T,
      Params => Null_T);
   --
   -- Accumulating the conjunctive (intersected, strictest)
   -- bounds on stack usage or final stack height.


   procedure Restrict_Stacks (
      Assertion : in     Assertion_T;
      Param     : in     Null_T;
      Total     : in out Stack_Bounds_T)
   --
   -- Restricts the Total stack bounds by the bounds from
   -- a stack Assertion.
   --
   -- Precondition: Assertion.Fact.Kind is Stack_Final or Stack_Usage.
   --
   is
      use Storage.Bounds;

      Interval : Interval_T renames Total(Assertion.Fact.Stack_Index);
      -- The bounds on the stack named in the assertion.

   begin

      Interval := Interval and Assertion.Fact.Stack_Value;

   end Restrict_Stacks;


   function Default_Stack_Usage (Subprogram : Programs.Subprogram_T)
   return Stack_Bounds_T
   --
   -- The default (unasserted) bounds on stack usage for the
   -- given Subprogram (which may influence the Initial_Stack_Height)
   -- for all stacks in this program.
   --
   is

      Stacks : Programs.Stacks_T :=
         Programs.Stacks (Within => Programs.Program (Subprogram));
      -- All the stacks in the program.

      Usage : Stack_Bounds_T (Stacks'Range);
      -- The default (unasserted) stack-usage bounds.

   begin

      for S in Stacks'Range loop

         Usage(S) := (
            Min => Storage.Bounds.Limit (Programs.Initial_Stack_Height (
               Stack    => Stacks(S),
               Entering => Subprogram)),
            Max => Storage.Bounds.Not_Limited);

      end loop;

      return Usage;

   end Default_Stack_Usage;


   function Default_Final_Stack_Height (Program : in Programs.Program_T)
   return Stack_Bounds_T
   --
   -- The default (unasserted) bounds on the final stack heights for
   -- all stacks in the given Program.
   --
   is

      Stacks : Programs.Stacks_T := Programs.Stacks (Program);
      -- All the stacks in the program.

      Final : Stack_Bounds_T (Stacks'Range);
      -- The default (unasserted) final-height bounds.

   begin

      for S in Stacks'Range loop

         case Programs.Kind (Stacks(S)) is

         when Programs.Stable =>

            Final(S) := Storage.Bounds.Exactly_Zero;

         when Programs.Unstable =>

            Final(S) := Storage.Bounds.Universal_Interval;

         end case;

      end loop;

      return Final;

   end Default_Final_Stack_Height;


   -- Property bounds


   package Value_Bounds is new Cumulations (
      Result => Storage.Bounds.Interval_T,
      Params => Null_T);
   --
   -- Accumulating the conjunctive (intersected, strictest)
   -- bounds on some values (of variables or properties). 


   procedure Restrict_Property (
      Assertion : in     Assertion_T;
      Param     : in     Null_T;
      Total     : in out Storage.Bounds.Interval_T)
   --
   -- Restricts the Total bounds on some property by the
   -- bounds from a property-value Assertion.
   --
   -- Precondition: Assertion.Fact.Kind = Property_Value.
   --
   is
      use type Storage.Bounds.Interval_T;
   begin

      Total := Total and Assertion.Fact.Prop_Value;

   end Restrict_Property;


   -- Execution or repetition count bounds:


   package Count_Bounds is new Cumulations (
      Result => Flow.Execution.Bound_T,
      Params => Null_T);
   --
   -- Accumulating the conjunctive (intersected, strictest)
   -- bounds on some values (of variables or properties). 


   procedure Restrict_Count (
      Assertion : in     Assertion_T;
      Param     : in     Null_T;
      Total     : in out Flow.Execution.Bound_T)
   --
   -- Restricts the Total execution-count or repetition-count bounds
   -- by the bounds from an execution-time Assertion.
   --
   -- Precondition: Assertion.Fact.Kind is Loop_Repetitions,
   -- Loop_Starts or Call_Executions (that is, Assertions.Fact.Count
   -- exists).
   --
   is
      use type Flow.Execution.Bound_T;
   begin

      Total := Total and Assertion.Fact.Count;

   end Restrict_Count;


   --
   ---   Reporting contradictory assertions
   --


   procedure Report_Conflict (
      Text   : in String;
      Subset : in Assertion_Subset_T;
      Within : in Assertion_Set_T;
      Locus  : in Output.Locus_T)
   --
   -- Reports the assertions in the Subset as Error:Conflict lines
   -- at the given Locus.
   --
   is

      Ass : Assertion_T;
      -- One of the Subset assertions.

   begin

      Output.Error (Locus, Text);

      for S in Subset'Range loop

         Ass := Element (Assertion_Bag_T (Within.all), Subset(S));

         Output.Error (
            Locus => Locus,
            Text  =>
              "Conflict"
            & Output.Field_Separator
            & Image (Ass.Source));

      end loop;

   end Report_Conflict;


   --
   ---   Applying assertion sets to various program parts under analysis
   --


   function Global_Values (
      Asserts : Assertion_Set_T)
   return Storage.Bounds.Var_Interval_List_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Program,
               Facts => +Variable_Value));
      -- The global variable-value assertions.

   begin

      return Var_Interval_Projections.Projection (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.all),
         Projector => To_Var_Interval'Access,
         Param     => (null record));

   end Global_Values;


   function Global_Values (
      Asserts : Assertion_Set_T;
      Point   : Processor.Code_Address_T)
   return Storage.Bounds.Cell_Interval_List_T
   is
   begin

      return
         Storage.Bounds.Cell_Intervals (
            From  => Global_Values (Asserts),
            Point => Point);

   end Global_Values;


   function Subprogram_Inputs (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Storage.Bounds.Var_Interval_List_T
   is

      Subset : constant Assertion_Subset_T :=
         Subprogram_Facts_Span_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Subprogram_Fact_Span'Access,
            Param  => (
               Subprogram => Subprogram,
               Model      => null,
               Facts      => +Variable_Value,
               Span       => Initially));
      -- The variable-value assertions at the start of this Subprogram.

   begin

      return Var_Interval_Projections.Projection (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.all),
         Projector => To_Var_Interval'Access,
         Param     => (null record));

   end Subprogram_Inputs;


   function Subprogram_Inputs (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Storage.Bounds.Cell_Interval_List_T
   is
   begin

      return
         Storage.Bounds.Cell_Intervals (
            From  => Subprogram_Inputs (Subprogram, Asserts),
            Point => Programs.Entry_Address (Subprogram));

   end Subprogram_Inputs;


   function Subprogram_Values (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Storage.Bounds.Var_Interval_List_T
   is

      Subset : constant Assertion_Subset_T :=
         Subprogram_Facts_Span_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Subprogram_Fact_Span'Access,
            Param  => (
               Subprogram => Subprogram,
               Model      => null,
               Facts      => +Variable_Value,
               Span       => Always));
      -- The variable-value assertions for all of this Subprogram.

   begin

      return Var_Interval_Projections.Projection (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.all),
         Projector => To_Var_Interval'Access,
         Param     => (null record));

   end Subprogram_Values;


   function Subprogram_Values (
      Subprogram : Programs.Subprogram_T;
      Point      : Processor.Code_Address_T;
      Asserts    : Assertion_Set_T)
   return Storage.Bounds.Cell_Interval_List_T
   is
   begin

      return
         Storage.Bounds.Cell_Intervals (
            From  => Subprogram_Values (Subprogram, Asserts),
            Point => Point);

   end Subprogram_Values;


   function Subprogram_Invariants (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Storage.Cell_Set_T
   is

      Subset : constant Assertion_Subset_T :=
         Subprogram_Facts_Span_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Subprogram_Fact_Span'Access,
            Param  => (
               Subprogram => Subprogram,
               Model      => null,
               Facts      => +Variable_Invariance,
               Span       => Always));
      -- The variable-invariance assertions for this Subprogram.
      -- Param.Span is irrelevant here.

      Invariants : Cell_Set_T;
      -- The collected cells, initially none.

   begin

      Invariant_Cell_Sets.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.all),
         Cumulator => Collect_Cell_Set'Access,
         Param     => (null record),
         Total     => Invariants);

      return Invariants;

   end Subprogram_Invariants;


   function Subprogram_Time_Bounded (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Boolean
   is
      use type Processor.Time_T;

      Time : constant Time_Bound_T := Subprogram_Time (Subprogram, Asserts);
      -- The asserted bounds on the time.

   begin

      return Time.Max < Processor.Time_T'Last;

      -- This is double work: the Subprogram_Time will probably be
      -- accessed twice in the analysis, first to check if it *is*
      -- asserted, then to insert the asserted bounds in the execution
      -- bounds of the subprogram. Oh well.

   end Subprogram_Time_Bounded;


   function Subprogram_Time (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Time_Bound_T
   is

      Subset : constant Assertion_Subset_T :=
         Subprogram_Facts_Span_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Subprogram_Fact_Span'Access,
            Param  => (
               Subprogram => Subprogram,
               Model      => null,
               Facts      => +Execution_Time,
               Span       => Always));
      -- The execution-time assertions for this Subprogram.
      -- Param.Span is irrelevant here.

      Time : Time_Bound_T := No_Bound;
      -- The cumulative (intersected) time bounds.

   begin

      Execution_Time_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.all),
         Cumulator => Restrict_Time'Access,
         Param     => (null record),
         Total     => Time);

      return Time;

   end Subprogram_Time;


   function Stacks_Bounded (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Boolean
   is

      Program : constant Programs.Program_T := Programs.Program (Subprogram);
      -- The program under analysis.

      Usage : constant Stack_Bounds_T := Stack_Usage (Subprogram, Asserts);
      -- The asserted bounds on stack usage.

      Final : constant Stack_Bounds_T :=
         Final_Stack_Height (Subprogram, Asserts);
      -- The asserted bounds on final stack height.

   begin

      -- Check the usage:

      for U in Usage'Range loop

         Output.Note (
              "Asserted usage for "
            & Programs.Stack_Name (U, Program)
            & Output.Field_Separator
            & Storage.Bounds.Image (Usage(U), "usage"));

         if Storage.Bounds.Unlimited (Usage(U).Max) then
            -- No upper bound on the usage of this stack.

            return False;

         end if;

      end loop;

      -- Check the final height:

      for F in Final'Range loop

         Output.Note (
              "Asserted final height for "
            & Programs.Stack_Name (F, Program)
            & Output.Field_Separator
            & Storage.Bounds.Image (
                 Item => Final(F),
                 Cell => Programs.Stack_Height (F, Program)));

         if not Storage.Bounds.Singular (Final(F)) then
            -- Fuzzy value or no bounds for the final stack height.

            return False;

         end if;

      end loop;

      -- All bounded, it seems.

      return True;

   end Stacks_Bounded;


   function Stack_Usage (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Stack_Bounds_T
   is

      Num_Stacks : constant Natural :=
         Programs.Number_Of_Stacks (Programs.Program (Subprogram));
      -- The number of stacks in the program.

      Subset : constant Assertion_Subset_T :=
         Subprogram_Facts_Span_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Subprogram_Fact_Span'Access,
            Param  => (
               Subprogram => Subprogram,
               Model      => null,
               Facts      => +Stack_Usage,
               Span       => Always));
      -- The stack-usage assertions for this Subprogram.
      -- Param.Span is irrelevant here.

      Usage : Stack_Bounds_T (1 .. Num_Stacks) :=
         Default_Stack_Usage (Subprogram);
      -- The cumulative (intersected) stack-usage bounds,
      -- initialized to the default (non-asserted) bounds.

   begin

      -- Asserted bounds on usage:

      Stack_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.all),
         Cumulator => Restrict_Stacks'Access,
         Param     => (null record),
         Total     => Usage);

      return Usage;

   end Stack_Usage;


   function Final_Stack_Height (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Stack_Bounds_T
   is

      Num_Stacks : constant Natural :=
         Programs.Number_Of_Stacks (Programs.Program (Subprogram));
      -- The number of stacks in the program.

      Subset : constant Assertion_Subset_T :=
         Subprogram_Facts_Span_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Subprogram_Fact_Span'Access,
            Param  => (
               Subprogram => Subprogram,
               Model      => null,
               Facts      => +Stack_Final,
               Span       => Always));
      -- The assertions on final stack height for this Subprogram.
      -- Param.Span is irrelevant here.

      Final : Stack_Bounds_T (1 .. Num_Stacks) :=
         Default_Final_Stack_Height (Programs.Program (Subprogram));
      -- The cumulative (intersected) final stack height bounds,
      -- initialized to the default (non-asserted) bounds.

   begin

      -- Asserted bounds on final height:

      Stack_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.all),
         Cumulator => Restrict_Stacks'Access,
         Param     => (null record),
         Total     => Final);

      return Final;

   end Final_Stack_Height;


   function Subprogram_Enough_For_Time (
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Boolean
   is

      Subset : constant Assertion_Subset_T :=
         Subprogram_Facts_Span_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Subprogram_Fact_Span'Access,
            Param  => (
               Subprogram => Subprogram,
               Model      => null,
               Facts      => +Subprogram_Enough_For_Time,
               Span       => Always));
      -- The "[not] enough for time" assertions for this Subprogram.
      -- Param.Span is irrelevant here.

      Assertion : Assertion_T;
      -- One of the Subset assertions.

      Final : Boolean := False;
      -- The final choice.
      -- The default is "not enough for time".

   begin

      -- Scan and accumulate the assertions:

      for S in Subset'Range loop

         Assertion := Element (Assertion_Bag_T (Asserts.all), Subset(S));

         if S > Subset'First
         and then Final /= Assertion.Fact.Enough_For_Time
         then

            Output.Warning ("Conflicting ""enough for time"" assertions.");

            -- TBA display the conflicting assertions.

         end if;

         Final := Assertion.Fact.Enough_For_Time;

      end loop;

      -- Return final choice:

      return Final;

   end Subprogram_Enough_For_Time;


   function Program_Has_Property (
      Property : Processor.Property_T;
      Asserts  : Assertion_Set_T)
   return Boolean
   is

      Subset : constant Assertion_Subset_T :=
         Part_Property_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Part_Property'Access,
            Param  => (
               Part     => Program,
               Property => Property));
      -- The global assertions on this Property.

   begin

      return Subset'Length > 0;

   end Program_Has_Property;


   function Subprogram_Has_Property (
      Property   : Processor.Property_T;
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Boolean
   is

      Subset : constant Assertion_Subset_T :=
         Subprogram_Property_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Subprogram_Property'Access,
            Param  => (
               Subprogram => Subprogram,
               Model      => null,
               Property   => Property));
      -- The assertions on this Property for this Subprogram.

   begin

      return Subset'Length > 0;

   end Subprogram_Has_Property;


   function Program_Property (
      Property : Processor.Property_T;
      Asserts  : Assertion_Set_T)
   return Storage.Bounds.Interval_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Property_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Part_Property'Access,
            Param  => (
               Part     => Program,
               Property => Property));
      -- The global assertions on this Property.

      Interval : Storage.Bounds.Interval_T :=
         Storage.Bounds.Universal_Interval;
      -- The intersection of all bounds from the Subset.

   begin

      if Subset'Length = 0 then

         raise No_Such_Property;

      end if;

      Value_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.all),
         Cumulator => Restrict_Property'Access,
         Param     => (null record),
         Total     => Interval);

      return Interval;

   end Program_Property;


   function Subprogram_Property (
      Property   : Processor.Property_T;
      Subprogram : Programs.Subprogram_T;
      Asserts    : Assertion_Set_T)
   return Storage.Bounds.Interval_T
   is

      Subset : constant Assertion_Subset_T :=
         Subprogram_Property_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Subprogram_Property'Access,
            Param  => (
               Subprogram => Subprogram,
               Model      => null,
               Property   => Property));
      -- The assertions on this Property in this Subprogram.

      Interval : Storage.Bounds.Interval_T :=
         Storage.Bounds.Universal_Interval;
      -- The intersection of all bounds from the Subset.

   begin

      if Subset'Length = 0 then

         raise No_Such_Property;

      end if;

      Value_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.all),
         Cumulator => Restrict_Property'Access,
         Param     => (null record),
         Total     => Interval);

      return Interval;

   end Subprogram_Property;


   --
   ---   Finding Call_Callee assertions on dynamic calls
   --


   function Image (
      Item   : Assertion_Subset_T;
      Within : Assertion_Set_T)
   return String
   --
   -- An adapted parameter profile.
   --
   is
   begin

      return Image (
         Item   => Item,
         Within => Assertion_Bag_T (Within.all));

   end Image;


   function Match_Dynamic_Callees (
      Assertion : Assertion_T;
      Param     : Actual_Part_T)
   return Boolean
   --
   -- Whether the Assertion applies to the actual part that is
   -- the Param (which is a Call_Dynamic) and moreover the Assertion
   -- gives the possible callees of this dynamic call.
   --
   is
   begin

      if Assertion.Parts.Kind /= Param.Kind
      or Assertion.Fact.Kind  /= Call_Callees
      then
         -- Evidently no match.

         return False;

      else

         if Opt.Trace_Matching then

            Output.Trace (
                 "Matching "
               & Image (Param)
               & " to callees assertion:");

            Text.Put (Assertion);

         end if;

         return Match (
            Part      => Param,
            Predicate => Assertion.Parts.Predicate,
            Goals     => No_Goals);

      end if;

   end Match_Dynamic_Callees;


   package Subprogram_Vectors is new Unbounded_Vectors (
      Element_Type   => Programs.Subprogram_T,
      Vector_Type    => Programs.Subprogram_List_T,
      Initial_Size   => 50,
      Size_Increment => 50,
      Deallocate     => Opt.Deallocate);


   type Unbounded_Subprogram_List_T
      is new Subprogram_Vectors.Unbounded_Vector;
   --
   -- For collecting callee lists from several Call_Callees facts.


   package Callees_Cumulate is new Cumulations (
      Result => Unbounded_Subprogram_List_T,
      Params => Null_T);
   --
   -- For collecting callee lists from several Call_Callees facts.


   procedure Collect_Callees (
      Assertion : in     Assertion_T;
      Param     : in     Null_T;
      Total     : in out Unbounded_Subprogram_List_T)
   --
   -- Collects the callee lists from all Call_Callees assertions
   -- that apply to one dynamic call.
   --
   is

      Index : Positive;
      -- The index of a new callee in Total. Not used.

      Callees : Programs.Subprogram_List_T renames Assertion.Fact.Callees.all;
      -- The asserted list of callees.

   begin

     for C in Callees'Range loop

        Find_Or_Add (
           Value  => Callees(C),
           Vector => Total,
           Index  => Index);

      end loop;

   end Collect_Callees;


   function Callees (
      Call    : Flow.Dynamic_Edge_T;
      From    : Programs.Subprogram_T;
      Model   : Flow.Computation.Model_Handle_T;
      Asserts : Assertion_Set_T)
   return Programs.Subprogram_List_T
   is

      Subset : constant Assertion_Subset_T :=
         Actual_Part_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Dynamic_Callees'Access,
            Param  => (
               Kind           => Call_Dynamic,
               Subprogram     => From,
               Model          => Model,
               Boundable_Call => Call));
      -- The callee assertions for this dynamic Call.

      Callee_Set : Unbounded_Subprogram_List_T;
      -- Collects the callees.

   begin

      if Opt.Trace_Map then

         Output.Trace (
            Locus => Flow.Show.Locus (
               Edge   => Call.all,
               Source => Programs.Symbol_Table (From)),
            Text  => "Assertions applied to dynamic call"
               & Output.Field_Separator
               & Image (Subset, Asserts));

      end if;

      Callees_Cumulate.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.all),
         Cumulator => Collect_Callees'Access,
         Param     => (null record),
         Total     => Callee_Set);

      declare

         Callee_List : constant Programs.Subprogram_List_T :=
            To_Vector (Callee_Set);
         -- The callees as a list.

      begin

         Erase (Callee_Set);

         return Callee_List;

      end;

   end Callees;


   --
   ---   Finding execution-count assertions on instructions
   --


   function Global_Or_Local (
      Predicate : Part_Predicate_Ref;
      Container : Programs.Subprogram_T;
      Model     : Flow.Computation.Model_Handle_T)
   return Boolean
   --
   -- Whether the Predicate is void (true) or identifies the given
   -- subprogram as an immediate container. This is a specialised
   -- "matching" function for instruction-repetition assertions
   -- applied within a given Container subprogram.
   --
   is
   begin

      if      Predicate = null
      or else Predicate'Length = 0
      then
         -- A global assertion, valid also within this Subprogram.

         return True;

      elsif Predicate'Length > 1 then
         -- Should never happen, now.

         Output.Fault (
            Location => "Assertions.Global_Or_Local",
            Text     => "Complex predicate");

         return False;

      else
        -- The predicate has a single feature.

        declare

           Feature : Feature_T renames Predicate(Predicate'First);

        begin

           return Feature.Kind = Within
           and then Match (
              Part  => (
                 Kind       => Subprogram,
                 Subprogram => Container,
                 Model      => Model),
              Parts => Feature.Whole.all,
              Goals => No_Goals);

        end;

      end if;

   end Global_Or_Local;


   function Instruction_Counts (
      Subprogram : Programs.Subprogram_T;
      Model      : Flow.Computation.Model_Handle_T;
      Asserts    : Assertion_Set_T)
   return Flow.Execution.Node_Count_Bounds_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Instruction,
               Facts => +Instruction_Executions));
      -- All the assertions on the execution count of an instruction,
      -- both global and local to any subprogram.

      Graph : constant Flow.Graph_T := Programs.Flow_Graph (Subprogram);
      -- The flow-graph of the subprogram.

      Num_Nodes : constant Positive := Positive (Flow.Max_Node (Graph));
      -- The total number of nodes in the Graph.

      Result : Flow.Execution.Node_Count_Bounds_T (1 .. Num_Nodes);
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).


      procedure Add_Bound (
         Node   : in     Flow.Node_T;
         Bound  : in     Flow.Execution.Bound_T;
         Voided :    out Boolean)
      --
      -- Adds the Bound on this Node to the Result.
      -- The Voided parameter shows if the result became a
      -- void interval (a contradiction).
      --
      is
         use type Flow.Node_T;
         use type Flow.Execution.Bound_T;

         N : Positive;
         -- The index of the Node in the Result.

         Was_Void : Boolean;
         -- Whether the node already had a void bound.

      begin

         -- Find the Node in the Result:

         N := 1;

         while N <= Last and then Result(N).Node /= Node loop

            N := N + 1;

         end loop;

         if N > Last then
            -- The Node is not in the Result yet. Add it.

            Last := Last + 1;

            Result(Last) := (
               Node  => Node,
               Count => (
                  Min => 0,
                  Max => Flow.Execution.Infinite));

            N := Last;

         end if;

         -- Apply the Bound to Result(N).Count:

         Was_Void := Flow.Execution.Void (Result(N).Count);

         Result(N).Count := Result(N).Count and Bound;

         Voided := Flow.Execution.Void (Result(N).Count) and not Was_Void;

      end Add_Bound;


      procedure Bound_Instruction_Count (
         Assertion : in Assertion_T)
      --
      -- Adds the execution-count bound from this Assertion to the Result.
      --
      -- Preconditions:
      --    Assertion.Parts.Kind = Instruction
      --    Assertion.Fact.Kind  = Instruction_Executions
      --
      is

         Steps : constant Flow.Step_List_T := Flow.Steps_Containing (
            Address   => Assertion.Parts.Address,
            Within    => Graph,
            Calls_Too => False);
         -- The steps within this Subprogram's flow-graph that
         -- model an instruction with the given address, but
         -- excluding call steps.

         Void : Boolean;
         -- Whether the assertion resulted in void (contradictory)
         -- bounds on the number of executions of the step.

      begin

         if Opt.Trace_Map then

            for S in Steps'Range loop

               Output.Trace (
                  Locus => Flow.Show.Locus (
                     Step   => Steps(S),
                     Source => Programs.Symbol_Table (Subprogram)),
                  Text =>
                       "Assertion applies to step #"
                     & Flow.Step_Index_T'Image (Flow.Index (Steps(S)))
                     & Output.Field_Separator
                     & Image (Assertion.Source));

            end loop;

         end if;

         if Steps'Length = 1 then
            -- Acceptable.

            Add_Bound (
               Node   => Flow.Node_Containing (Steps(Steps'First), Graph),
               Bound  => Assertion.Fact.Count,
               Voided => Void);

            if Void then
               -- The result is an infeasible constraint, an empty
               -- set of execution counts.

               if Flow.Execution.Void (Assertion.Fact.Count) then

                  Output.Warning (
                     Locus => Own.Locus (Assertion.Source),
                     Text  => "Assertion is a contradiction.");

               else

                  Output.Warning (
                     Locus => Own.Locus (Assertion.Source),
                     Text  => "Assertion contradicts other assertions.");

               end if;

           end if;
 
         elsif Steps'Length > 0 then

            Output.Error (
                 "Instruction-repetition assertion applies to several steps"
               & Output.Field_Separator
               & Image (Assertion.Source));

         end if;

      end Bound_Instruction_Count;


      Assn : Assertion_T;
      -- One of the assertions in the Subset.


   begin  -- Instruction_Counts

      for S in Subset'Range loop

         Assn := Element (Assertion_Bag_T (Asserts.all), Subset(S));

         if Global_Or_Local (
            Predicate => Assn.Parts.Predicate,
            Container => Subprogram,
            Model     => Model)
         then
            -- This assertion applies in this Subprogram.

            Bound_Instruction_Count (Assn);

         end if;

      end loop;

      if Opt.Trace_Map then

         for R in 1 .. Last loop

            Output.Trace (
               Locus => Flow.Show.Locus (
                  Node => Result(R).Node,
                  Source => Programs.Symbol_Table (Subprogram)),
               Text =>
                    "Asserted execution count for node #"
                  & Flow.Node_Index_T'Image (Flow.Index (Result(R).Node))
                  & Output.Field_Separator
                  & Flow.Execution.Image (Result(R).Count));

         end loop;

      end if;

      return Result(1 .. Last);

   end Instruction_Counts;


   function Instruction_Role (
      Address : Processor.Code_Address_T;
      Asserts : Assertion_Set_T)
   return Processor.Instruction_Role_T
   is
      use type Processor.Instruction_Role_T;

      Subset : constant Assertion_Subset_T :=
         Instruction_Fact_Subsets.Subset (
            From   => Assertion_Bag_T (Asserts.all),
            Filter => Match_Instruction_Fact'Access,
            Param  => (
               Address => Address,
               Facts   => +Instruction_Role));
      -- All the assertions on the role of an instruction,
      -- both global and local to any subprogram.

      Assn : Assertion_T;
      -- One of the assertions in the Subset.

      Role : Processor.Instruction_Role_T := Processor.No_Role;
      -- The role that is asserted, or No_Role if there are
      -- no assertions on this.

   begin

      for S in Subset'Range loop

         Assn := Element (Assertion_Bag_T (Asserts.all), Subset(S));

         if Role = Processor.No_Role then
            -- This is the first role assertion.

            Role := Assn.Fact.Role;

         elsif Assn.Fact.Role /= Role then
            -- This is a conflicting role assertion.

            Output.Warning (
               Locus => Own.Locus (Assn.Source),
               Text  => "Assertion contradicts other assertions.");

            exit;

         end if;

      end loop;

      return Role;

   end Instruction_Role;


   --
   ---   Mapping applicable assertions to subprogram parts
   --


   type Loop_Assertions_T is
      array (Loops.Loop_Index_T range <>) of Assertion_Subset_Ref;
   --
   -- The subset of assertions that apply to a given loop, for
   -- all the loops in a given subprogram.


   type Call_Assertions_T is
      array (Programs.Call_Index_T range <>) of Assertion_Subset_Ref;
   --
   -- The subset of assertions that apply to a given call, for
   -- all the calls from a given subprogram.


   type Property_Table_Count_T is new Natural;
   --
   -- The number of property tables in an assertion map.
   -- As many tables are constructed as there are distinct "areas"
   -- (loops,calls) with property assertions.


   subtype Property_Table_Index_T is
      Property_Table_Count_T range 1 .. Property_Table_Count_T'Last;
   --
   -- Identifies one of the property-tables in an assertion map.


   type Property_Tables_T is
      array (Property_Table_Index_T range <>) of Property_Table_T;
   --
   -- The property tables in an assertion map.


   type Property_Map_T is
      array (Flow.Node_Index_T range <>) of Property_Table_Index_T;
   --
   -- Associates each node in a flow-graph with the relevant property
   -- table (tuple), which contains the most locally asserted property
   -- values for the node.


   type Assertion_Map_Object_T (
      Number_Of_Loops           : Loops.Loop_Count_T;
      Number_Of_Calls           : Natural;
      Number_Of_Property_Tables : Property_Table_Count_T;
      Max_Node_Index            : Flow.Node_Index_T)
   is record
      Set             : Assertion_Set_T;
      Model           : Flow.Computation.Model_Handle_T;
      On_Subprogram   : Assertion_Subset_Ref;
      To_Map          : Assertion_Subset_Ref;
      On_Loop         : Loop_Assertions_T (1 .. Number_Of_Loops);
      On_Call         : Call_Assertions_T (1 .. Number_Of_Calls);
      Property_Tables : Property_Tables_T (1 .. Number_Of_Property_Tables);
      Properties      : Property_Map_T    (1 .. Max_Node_Index);
   end record;
   --
   -- Maps the assertions from a given assertion set to the loops and
   -- calls in a given subprogram. The subprogram itself is not
   -- recorded here, but its current (context-specific) computation
   -- model is.
   --
   -- Set
   --    The underlying assertion set.
   -- Model
   --    The computation model for the subprogram.
   -- On_Subprogram
   --    The subset of assertions applicable to the subprogram as a
   --    whole (Parts.Kind = Subprogram and the Subprogram matches
   --    Parts.Predicate). Global assertions (Parts.Kind = Program)
   --    are not included.
   -- To_Map
   --    The subset of assertions that is applicable for mapping to
   --    the loops and calls in this subprogram.
   -- On_Loop
   --    The subset of assertions applicable to the loop, for each
   --    loop in the subprogram.
   -- On_Call
   --    The subset of assertions applicable to the call, for each
   --    call in the subprogram.
   -- Property_Tables
   --    The set of property tables defined by property assertions
   --    that apply to some or all parts (nodes) of the subprogram.
   -- Properties
   --    The asserted or defaulted property values for the node, for
   --    each node in the subprogram's flow-graph.
   -- 
   -- Some of the Property_Tables may be unused (at the end of the vector).
   -- The Property_Table_Index_T values in Properties will refer only to
   -- defined and valid elements in Property_Tables.


   package Part_Subsets is new Subsets (Params => Part_Kinds_T);
   --
   -- Choosing assertion subsets based on the kind of Part the
   -- assertion applies to; all kinds of Facts are accepted.


   function Part_Property (
      Assertion : Assertion_T;
      Param     : Part_Kinds_T)
   return Boolean
   --
   -- Whether the Assertion states bounds on the value of
   -- some property, over some parts of the desired kinds.
   --
   is
   begin

      return Param(Assertion.Parts.Kind)
         and Assertion.Fact.Kind = Property_Value;

   end Part_Property;


   procedure Map_Properties (
      Subprogram : in     Programs.Subprogram_T;
      Luups      : in     Loops.Loops_T;
      Map        : in out Assertion_Map_Object_T)
   --
   -- Creates the property map for a subprogram, based on the set of
   -- loops and the assertions that are applicable to each loop (as
   -- already recorded in the assertion map).
   --
   is

      Next_Table_Index : Property_Table_Index_T := Map.Property_Tables'First;
      -- The lowest unused index of a property table in the
      -- assertion map.

      Initial_Table : Property_Table_T := 
         Processor.Properties.Default_Properties (Subprogram);
      -- The properties that apply in the subprogram, outside of
      -- any loops with more specific assertions.
      -- Initialised to default properties, to be updated with
      -- global assertions and subprogram-level assertions.

      Initial_Index : Property_Table_Index_T;
      -- The index of Initial_Table in the map.

      Mapped : array (Luups'Range) of Boolean := (others => False);
      -- Records which loops have already been mapped.


      procedure Apply (
         Assertions : in     Assertion_Subset_T;
         To         : in out Property_Table_T)
      --
      -- Update the property-table with the Property_Values
      -- asserted in (some of) the given Assertions.
      -- Note that the new bound _replaces_ the tabled bound;
      -- there is no _intersection_ of bounds. Thus, a property
      -- assertion in a loop can completely override an assertion
      -- for the same property from a higher level. However, within
      -- the given Assertions subset the bounds on the same property
      -- are first intersected, before the To table is updated.
      --
      is
         use type Storage.Bounds.Interval_T;

         New_Table : Property_Table_T := (
            others => Storage.Bounds.Universal_Interval);
         -- The table of new property values from Assertions.

         Asserted : array (Processor.Property_T) of Boolean := (
            others => False);
         -- Whether a given property is asserted in Assertions.

         Fact : Fact_T;
         -- One of the facts from the Assertions.

      begin

         -- Compute New_Table from the Assertions, taking
         -- the intersection of  multiple bounds for the
         -- same property:

         for A in Assertions'Range loop

            Fact := Element (Map.Set.all, Assertions(A)).Fact;

            if Fact.Kind = Property_Value then

               New_Table(Fact.Property) := 
                  New_Table(Fact.Property) and Fact.Prop_Value;

               Asserted(Fact.Property) := True;

            end if;

         end loop;

         -- Update To with New_Table where Asserted:

         for P in Processor.Property_T loop

            if Asserted(P) then

               To(P) := New_Table(P);

            end if;

         end loop;

      end Apply;


      procedure Add_To_Map (
         Table : in     Property_Table_T;
         Index :    out Property_Table_Index_T)
      --
      -- Adds a property table to the assertion map and
      -- returns its index.
      --
      is
      begin

         Index := Next_Table_Index;

         Map.Property_Tables (Index) := Table;

         Next_Table_Index := Next_Table_Index;

      end Add_To_Map;


      procedure Map_Loop (
         Root    : Loops.Loop_T;
         Inherit : Property_Table_T)
      --
      -- Maps properties into the loop hierarchy rooted at
      -- a given loop, with a given set of inherited (outer)
      -- property bounds.
      -- Applies recursively to nested loops.
      --
      -- Each loop is given its own property table, whether or not
      -- some properties are asserted specifically for that loop.
      -- This is a little wasteful, but we don't expect zillions of
      -- properties.
      --
      is

         Loop_Table : Property_Table_T := Inherit;
         -- The loop-level properties.
         -- Initialised to the inherited properties, to be
         -- updated with the assertions on the Root loop.

         Loop_Table_Index : Property_Table_Index_T;
         -- The index of Loop_Table in the map.

         Members : constant Flow.Node_Index_List_T :=
            Flow.To_Index_List (Loops.Members (Root).all);
         -- The indices of the nodes in the Root loop.
         -- Includes nodes in nested loops, but would not need to.

         Inner_Loops : constant Loops.Loop_List_T :=
            Loops.Loops_Contained_In (Loops => Luups, Luup => Root);
         -- The loops nested immediately in the Root loop.

      begin

         -- Apply the assertions on the Root loop:

         Apply (
            Assertions => Part_Subsets.Subset (
               From   => Map.On_Loop(Loops.Index (Root)).all,
               Within => Assertion_Bag_T (Map.Set.all),
               Filter => Part_Property'Access,
               Param  => +Luup),
            To => Loop_Table);

         Add_To_Map (
            Table => Loop_Table,
            Index => Loop_Table_Index);

         -- Map the Loop_Table on all member nodes:

         for M in Members'Range loop

            Map.Properties (Members (M)) := Loop_Table_Index;

         end loop;

         Mapped (Loops.Loop_Index (Root)) := True;

         -- Recurse to inner loops:

         for I in Inner_Loops'Range loop

            Map_Loop (
               Root    => Inner_Loops (I),
               Inherit => Loop_Table);

         end loop;

      end Map_Loop;


   begin  -- Map_Properties

      -- Apply global assertions to the Initial table:

      Apply (
         Assertions => Part_Subsets.Subset (
            From   => Assertion_Bag_T (Map.Set.all),
            Filter => Part_Property'Access,
            Param  => +Program),
         To => Initial_Table);

      -- Then apply subprogram-level assertions, overriding
      -- the global assertions in the Initial table:

      Apply (
         Assertions => Part_Subsets.Subset (
            From   => Map.On_Subprogram.all,
            Within => Assertion_Bag_T (Map.Set.all),
            Filter => Part_Property'Access,
            Param  => +Program),
         To => Initial_Table);

      -- Initialise the property map to the initial table:

      Add_To_Map (Table => Initial_Table, Index => Initial_Index);

      for N in Map.Properties'Range loop

         Map.Properties(N) := Initial_Index;

      end loop;

      -- Map each loop, recursing from outer loops to inner loops:

      for L in reverse Luups'Range loop

         if not Mapped (L) then

            -- This is an outermost loop.

            Map_Loop (
               Root    => Luups(L),
               Inherit => Initial_Table);

         -- else
         --    The loop is nested in some outermost loop that
         --    we already mapped, and so this nested loop was
         --    also mapped by the recursive Map_Loop.

         end if;

      end loop;      

   end Map_Properties;


   function Image (
      Item   : Assertion_Subset_Ref;
      Within : Assertion_Set_T)
   return String
   --
   -- A common parameter profile for the Image of an assertion subset.
   --
   is
   begin

      return Image (
         Item   => Item.all,
         Within => Assertion_Bag_T (Within.all));

   end Image;


   procedure Identify_Assertions (
      Model         : in     Flow.Computation.Model_Handle_T;
      Assertion_Set : in     Assertion_Set_T;
      Assertion_Map :    out Assertion_Map_T)
   is

      Subprogram : constant Programs.Subprogram_T :=
         Flow.Computation.Subprogram (Model.all);
      -- The subprogram under analysis, for which the computation
      -- Model was built.

      Luups : constant Loops.Loops_T := Programs.Loops_Of (Subprogram);
      -- The loop structure of the Subprogram.

      Calls : constant Programs.Call_List_T := Programs.Calls_From (Subprogram);
      -- The calls from the Subprogram.

      Map : constant Assertion_Map_T := new Assertion_Map_Object_T (
         Number_Of_Loops           => Luups'Length,
         Number_Of_Calls           => Calls'Length,
         Number_Of_Property_Tables => Luups'Length + 1,
         Max_Node_Index            =>
            Flow.Max_Node (Programs.Flow_Graph (Subprogram)));
      --
      -- The result.

      Object : Assertion_Set_Object_T renames Assertion_Set.all;
      -- The assertion set object.

      To_Map : constant Assertion_Subset_T :=
         To_Be_Mapped (
            From => Assertion_Bag_T (Object),
            Onto => Subprogram);
      -- The assertions that should be mapped.
      -- These assertions concern only mappable parts.


      procedure Map_Loop (
         Luup   : in     Loops.Loop_T;
         Giving :    out Assertion_Subset_Ref)
      is

         Loop_Mark : Output.Nest_Mark_T;
         -- The default output locus is for the Luup.

      begin

         Loop_Mark := Output.Nest (Programs.Locus (Luup, Subprogram));

         Giving := new Assertion_Subset_T'(
            Actual_Part_Subsets.Subset (
               From   => Assertion_Bag_T (Object),
               Filter => Match_Actual_Part'Access,
               Param  => (
                  Kind       => Own.Luup,
                  Subprogram => Subprogram,
                  Model      => Model,
                  Luup       => Luup)));

         if Opt.Trace_Map then

            Output.Trace (
                  "Assertions mapped on loop"
                & Output.Field_Separator
                & Image (Giving, Assertion_Set));

         end if;

         Output.Unnest (Loop_Mark);

      exception

      when X : others =>

         Output.Exception_Info (
            Text       => "Assertions.Identify_Assertions.Map_Loop",
            Occurrence => X);

         Giving := Empty_Subset;

         Output.Unnest (Loop_Mark);

      end Map_Loop;


      procedure Map_Call (
         Call   : in     Programs.Call_T;
         Giving :    out Assertion_Subset_Ref)
      is

         Call_Mark : Output.Nest_Mark_T;
         -- The default output locus is for the Call.

      begin

         Call_Mark := Output.Nest (Programs.Locus (Call));

         Giving := new Assertion_Subset_T'(
            Actual_Part_Subsets.Subset (
               From   => Assertion_Bag_T (Object),
               Filter => Match_Actual_Part'Access,
               Param  => (
                  Kind       => Call_Static,
                  Subprogram => Subprogram,
                  Model      => Model,
                  Call       => Call)));

         if Opt.Trace_Map then

            Output.Trace (
                 "Assertions mapped on call"
               & Output.Field_Separator
               & Image (Giving, Assertion_Set));

         end if;

         Output.Unnest (Call_Mark);

      exception

      when X : others =>

         Output.Exception_Info (
            Text       => "Assertions.Identify_Assertions.Map_Call",
            Occurrence => X);

         Giving := Empty_Subset;

         Output.Unnest (Call_Mark);

      end Map_Call;


   begin  -- Identify_Assertions

      if Opt.Trace_Matching then

         Output.Trace ("Assertion matching starts.");

      end if;

      if Opt.Trace_Map
      or Opt.Trace_To_Be_Mapped
      then

         Output.Trace (
              "Assertions to map"
           & Output.Field_Separator
           & Image (To_Map, Assertion_Set));

      end if;

      -- Create the assertion map object:

      Map.Set := Assertion_Set;

      Map.Model := Model;

      -- Map the assertions:

      Map.On_Subprogram := new Assertion_Subset_T'(
         Actual_Part_Subsets.Subset (
            From   => Assertion_Bag_T (Object),
            Filter => Match_Actual_Part'Access,
            Param  => (
               Kind       => Own.Subprogram,
               Subprogram => Subprogram,
               Model      => Model)));

      if Opt.Trace_Map then

         Output.Trace (
              "Assertions mapped on subprogram"
            & Output.Field_Separator
            & Image (Map.On_Subprogram, Assertion_Set));

      end if;

      if To_Map'Length > 0 then
         -- There are some assertions that may be mapped
         -- onto loops and calls in this Subprogram.

         Map.To_Map := new Assertion_Subset_T'(To_Map);

         for L in Luups'Range loop

            Map_Loop (
               Luup   => Luups(L),
               Giving => Map.On_Loop(Loops.Index (Luups(L))));

         end loop;

         for C in Calls'Range loop

            Map_Call (
               Call   => Calls(C),
               Giving => Map.On_Call(Programs.Index (Calls(C))));

         end loop;

      else
         -- There are no assertions to map onto the loops
         -- and calls in this subprogram.

         Map.To_Map  := Empty_Subset;
         Map.On_Loop := (others => Empty_Subset);
         Map.On_Call := (others => Empty_Subset);

      end if;

      Map_Properties (
         Subprogram => Subprogram,
         Luups      => Luups,
         Map        => Map.all);

      if Opt.Trace_Matching then

         Output.Trace ("Assertion matching ends.");

      end if;

      Assertion_Map := Map;

   end Identify_Assertions;


   procedure Verify_Map (
      Map   : in     Assertion_Map_T;
      Valid :    out Boolean)
   is

      Subprogram : constant Programs.Subprogram_T :=
         Flow.Computation.Subprogram (Map.Model.all);
      -- The subprogram for which the Map is made.

      Luups : constant Loops.Loops_T := Programs.Loops_Of (Subprogram);
      -- The loop structure of the Subprogram.

      Calls : constant Programs.Call_List_T := Programs.Calls_From (Subprogram);
      -- The calls from the Subprogram.

      Object : Assertion_Set_Object_T renames Map.Set.all;
      -- The assertion set object.

      subtype Ass_Index_T is Positive range First (Object) .. Last (Object);
      -- The indices of the assertions in the assertion set.

      Map_Tally : array (Ass_Index_T) of Natural := (others => 0);
      -- The number of parts (loops, calls) to which an assertion
      -- is mapped.


      procedure Tally (Mapped : Assertion_Subset_T)
      --
      -- Adds these Mapped assertions to Map_Tally.
      --
      is

         Index : Ass_Index_T;

      begin

         for M in Mapped'Range loop

            Index := Mapped(M);

            if not Is_Member (Index, Map.To_Map.all) then

               Output.Fault (
                  Location => "Assertions.Verify.Tally",
                  Text     =>
                       "Assertion #"
                     & Ass_Index_T'Image (Index)
                     & " is not to be mapped.");

            end if;

            Map_Tally(Index) := Map_Tally(Index) + 1;

         end loop;

      end Tally;


      procedure Warn_Infeasible_Assertions (
          Subset : in Assertion_Subset_Ref;
          Part   : in String;
          Locus  : in Output.Locus_T)
      --
      -- Given a Subset of assertions on an infeasible part, at
      -- the given program Locus, emits warnings for those assertions
      -- that are not simply "repeats 0 times".
      --
      is

         Ass : Assertion_T;
         -- An assertion in the Subset.

      begin

         for S in Subset'Range loop

            Ass := Element (Object, Subset(S));

            if Ass.Fact.Kind in Count_Fact_Kind_T
            and then (Flow.Execution.Is_In (0, Ass.Fact.Count)
                  or  Flow.Execution.Void  (   Ass.Fact.Count))
            then
               -- A Count assertion ("starts" or "repeats") that allows
               -- zero executions, which is compatible with an infeasible
               -- part, or is void (contradictory) and thus has been
               -- reported elsewhere (and may be the reason for the
               -- part being marked infeasible).

               null;

            else
               -- Some non-Count assertion, or a Count assertion
               -- that does not admit zero counts.

               Output.Warning (
                  Locus => Locus,
                  Text  =>
                       "Assertion on infeasible "
                     & Part
                     & " has no effect"
                     & Output.Field_Separator
                     & Image (Ass.Source));

            end if;

         end loop;

      end Warn_Infeasible_Assertions;


      function Wrong_Tally (
         Tally      : Arithmetic.Value_T;
         Population : Storage.Bounds.Interval_T)
      return String
      --
      -- Describes the Tally, which is not within the required
      -- interval of Population, as too "few" or too "many".
      --
      is
         use Storage.Bounds;
      begin

         if Tally < Population then return "few";
                               else return "many";
         end if;

      end Wrong_Tally;


      procedure Wrong_Loop_Tally (
         Assertion  : in Assertion_T;
         Index      : in Ass_Index_T;
         Tally      : in Arithmetic.Value_T;
         Population : in Storage.Bounds.Interval_T)
      --
      -- Emits the error "Loop matches too (many/few) entities",
      -- followed by error messages that list the loops that do
      -- match this Assertion, which has this Index.
      --
      is
         use type Output.Locus_T;

         Ass_Locus : constant Output.Locus_T := Locus (Assertion.Source);
         -- The locus of the assertion, in an assertion file.

         Num : Natural := 0;
         -- Numbers the matching loops.

      begin

         Output.Error (
            Locus => Ass_Locus,
            Text  =>
                 "Loop matches too "
               & Wrong_Tally (Tally, Population)
               & " ("
               & Arithmetic.Image (Tally)
               & ") entities in "
               & Programs.Name (Subprogram));

         for L in Luups'Range loop

            if Is_Member (Index, Map.On_Loop(Loops.Index (Luups(L))).all)
            then

               Num := Num + 1;

               Output.Error (
                  Locus => Ass_Locus,
                  Text  =>
                       "Match"
                     & Natural'Image (Num)
                     & Output.Image (Loops.Show.Locus (
                          Luup   => Luups(L),
                          Within => Programs.Flow_Graph (Subprogram),
                          Source => Programs.Symbol_Table (Subprogram))));

            end if;

         end loop;

      end Wrong_Loop_Tally;


      procedure Wrong_Call_Tally (
         Assertion  : in Assertion_T;
         Index      : in Ass_Index_T;
         Tally      : in Arithmetic.Value_T;
         Population : in Storage.Bounds.Interval_T)
      --
      -- Emits the error "Call matches too (many/few) entities",
      -- followed by error messages that list the calls that do
      -- match this Assertion, which has this Index.
      --
      is

         Ass_Locus : constant Output.Locus_T := Locus (Assertion.Source);
         -- The locus of the assertion, in an assertion file.

         Num : Natural := 0;
         -- Numbers the matching calls.

      begin

         Output.Error (
            Locus => Ass_Locus,
            Text  =>
                 "Call matches too "
               & Wrong_Tally (Tally, Population)
               & " ("
               & Arithmetic.Image (Tally)
               & ") entities in "
               & Programs.Name (Subprogram));

         for C in Calls'Range loop

            if Is_Member (Index, Map.On_Call(Programs.Index (Calls(C))).all)
            then

               Num := Num + 1;

               Output.Error (
                  Locus => Ass_Locus,
                  Text  =>
                       "Match"
                     & Natural'Image (Num)
                     & Output.Field_Separator
                     & Programs.Image (Calls(C)));

            end if;

         end loop;

      end Wrong_Call_Tally;


      procedure Check_Tally (
         Assertion : in Assertion_T;
         Index     : in Ass_Index_T;
         Tally     : in Arithmetic.Value_T)
      --
      -- Checks that the actually mapped Tally agrees with
      -- the expected population of the Assertion, which has
      -- the given Index and refers to a mappable part.
      -- If they disagree, sets Valid to False and reports
      -- errors.
      --
      is

         Pop : Storage.Bounds.Interval_T renames Assertion.Parts.Population;

      begin

         if Opt.Trace_Map then

            Output.Trace (
               Locus => Locus (Assertion.Source),
               Text  =>
                    "Assertion #"
                  & Ass_Index_T'Image (Index)
                  & " matches "
                  & Arithmetic.Image (Tally)
                  & " entities in "
                  & Programs.Name (Subprogram));

         end if;

         if not Storage.Bounds.Is_In (Tally, Pop) then

            Valid := False;

            case Mappable_Part_Kind_T (Assertion.Parts.Kind) is

            when Luup =>

               Wrong_Loop_Tally (
                  Assertion  => Assertion,
                  Index      => Index,
                  Tally      => Tally,
                  Population => Pop);
            
            when Call_Static =>

               Wrong_Call_Tally (
                  Assertion  => Assertion,
                  Index      => Index,
                  Tally      => Tally,
                  Population => Pop);

            end case;

         end if;

      end Check_Tally;


      Subset : Assertion_Subset_Ref;
      -- A subset of assertions mapped onto a loop or call.

      Ass_Index : Ass_Index_T;
      -- The index of an assertion being checked.

   begin  -- Verify_Map

      Valid := True;
      -- Initially we know nothing bad. We may learn some ...

      if Map.To_Map'Length > 0 then
         -- There are some assertions that may be mapped
         -- onto loops and calls in this Subprogram, so there
         -- is something to be verified.

         -- Tally the number of matching loops and calls, and
         -- check for mappings to infeasible loops or calls:

         for L in Luups'Range loop

            Subset := Map.On_Loop(Loops.Index (Luups(L)));

            Tally (Subset.all);

            if not Flow.Computation.Is_Feasible (Luups(L), Map.Model.all)
            then

               Warn_Infeasible_Assertions (
                  Subset => Subset,
                  Part   => "loop",
                  Locus  => Programs.Locus (Luups(L), Subprogram));

            end if;

         end loop;

         for C in Calls'Range loop

            Subset := Map.On_Call(Programs.Index (Calls(C)));

            Tally (Subset.all);

            if not Flow.Computation.Is_Feasible (Calls(C), Map.Model.all)
            then

               Warn_Infeasible_Assertions (
                  Subset => Subset,
                  Part   => "call",
                  Locus  => Programs.Locus (Calls(C)));

            end if;

         end loop;

         -- Check the tallies against the expected populations:

         for T in Map.To_Map'Range loop

            Ass_Index := Map.To_Map(T);

            Check_Tally (
               Assertion => Element (Object, Ass_Index),
               Index     => Ass_Index,
               Tally     => Arithmetic.Value_T (Map_Tally(Ass_Index)));

         end loop;

      end if;

   end Verify_Map;


   function Set (Map : Assertion_Map_T) return Assertion_Set_T
   is
   begin

      return Map.Set;

   end Set;


   function Subprogram (Map : Assertion_Map_T)
   return Programs.Subprogram_T
   is
   begin

      return Flow.Computation.Subprogram (Map.Model.all);

   end Subprogram;


   function Stack_By (
      Index : Programs.Stack_Index_T;
      Map   : Assertion_Map_T)
   return Programs.Stack_T
   is
   begin

      return Programs.Stack_By (
         Index => Index,
         Within => Programs.Program (Subprogram (Map)));

   end Stack_By;


   --
   ---   Applying assertion maps to various program parts under analysis
   --


   function Loop_Values (
      Luup    : Loops.Loop_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Var_Interval_List_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Asserts.On_Loop(Loops.Index (Luup)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Own.Luup,
               Facts => +Variable_Value));
      -- The assertions on variable values for this loop.

   begin

      return Var_Interval_Projections.Projection (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Projector => To_Var_Interval'Access,
         Param     => (null record));

   end Loop_Values;


   function Loop_Values (
      Luup    : Loops.Loop_T;
      Point   : Processor.Code_Address_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Cell_Interval_List_T
   is
   begin

      return
         Storage.Bounds.Cell_Intervals (
            From  => Loop_Values (Luup, Asserts),
            Point => Point);

   end Loop_Values;


   function Call_Values (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Var_Interval_List_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Asserts.On_Call(Programs.Index (Call)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Call_Static,
               Facts => +Variable_Value));
      -- The assertions on variable values for this loop.

   begin

      return Var_Interval_Projections.Projection (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Projector => To_Var_Interval'Access,
         Param     => (null record));

   end Call_Values;


   function Call_Values (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Cell_Interval_List_T
   is
   begin

      return
         Storage.Bounds.Cell_Intervals (
            From  => Call_Values (Call, Asserts),
            Point => Flow.Prime_Address (Programs.Step (Call)));

   end Call_Values;


   function Loop_Nest_Values (
      Luups   : Loops.Loop_List_T;
      Point   : Processor.Code_Address_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Cell_Interval_List_T
   is
      use type Storage.Bounds.Cell_Interval_List_T;
   begin

      if Luups'Length = 0 then

         return Storage.Bounds.Empty;

      elsif Luups'Length = 1 then

         return Loop_Values (
            Luup    => Luups(Luups'First),
            Point   => Point,
            Asserts => Asserts);

      else
         -- More than one loop in the list.

         return
            Loop_Values (
               Luup    => Luups(Luups'First),
               Point   => Point,
               Asserts => Asserts)
         and
            Loop_Nest_Values (
               Luups   => Luups(Luups'First + 1 .. Luups'Last),
               Point   => Point,
               Asserts => Asserts);

      end if;

   end Loop_Nest_Values;


   function Local_Call_Values (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Storage.Bounds.Cell_Interval_List_T
   is
      use Storage.Bounds;

      Caller : constant Programs.Subprogram_T := Programs.Caller (Call);
      -- The calling subprogram.

      Step : constant Flow.Step_T := Programs.Step (Call);
      -- The call step. This is the point at which variables
      -- are mapped to cells.

      Point : constant Processor.Code_Address_T := Flow.Prime_Address (Step);
      -- The point at which variables are mapped to cells.

      Sub_Bounds : constant Cell_Interval_List_T :=
         Subprogram_Values (Caller, Point, Asserts.Set);
      -- Bounds on cells asserted for the calling subprogram, at
      -- the point of call.

      Outer : constant Loops.Loop_List_T :=
         Loops.Containing_Loops (
            Loops => Loops.All_Loops (Programs.Loops_Of (Caller)),
            Node  => Programs.Node (Call));
      -- The loops, if any, that contain the Call.

      Loop_Bounds : constant Cell_Interval_List_T :=
         Loop_Nest_Values (Outer, Point, Asserts);
      -- Bounds asserted for the containing loops, at the point of call.

      Call_Bounds : constant Cell_Interval_List_T :=
         Call_Values (Call, Asserts);
      -- Bounds asserted for the call itself.

   begin

      return Sub_Bounds
         and Loop_Bounds
         and Call_Bounds;

   end Local_Call_Values;


   function Loop_Invariants (
      Luup    : Loops.Loop_T;
      Asserts : Assertion_Map_T)
   return Storage.Cell_Set_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Asserts.On_Loop(Loops.Index (Luup)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Own.Luup,
               Facts => +Variable_Invariance));
      -- The assertions on invariant variables for this loop.

      Invariants : Cell_Set_T;
      -- The collected cells, initially none.

   begin

      Invariant_Cell_Sets.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Cumulator => Collect_Cell_Set'Access,
         Param     => (null record),
         Total     => Invariants);

      return Invariants;

   end Loop_Invariants;


   function Call_Invariants (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Storage.Cell_Set_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Asserts.On_Call(Programs.Index (Call)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Call_Static,
               Facts => +Variable_Invariance));
      -- The assertions on invariant variables for this call.

      Invariants : Cell_Set_T;
      -- The collected cells, initially none.

   begin

      Invariant_Cell_Sets.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Cumulator => Collect_Cell_Set'Access,
         Param     => (null record),
         Total     => Invariants);

      return Invariants;

   end Call_Invariants;


   function Call_Invariants (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Storage.Cell_List_T
   is
   begin

      return Storage.To_List (Call_Invariants (Call, Asserts));

   end Call_Invariants;


   function Loop_Start (
      Luup    : Loops.Loop_T;
      Asserts : Assertion_Map_T)
   return Flow.Execution.Bound_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Asserts.On_Loop(Loops.Index (Luup)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Own.Luup,
               Facts => +Loop_Starts));
      -- The assertions on the number of starts for this loop.

      Count : Flow.Execution.Bound_T := Flow.Execution.Unbounded;
      -- The intersection of all start-count assertions.

   begin

      Count_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Cumulator => Restrict_Count'Access,
         Param     => (null record),
         Total     => Count);

      if Flow.Execution.Void (Count) then

         Report_Conflict (
            Text   => "Conflicting assertions on loop starts",
            Subset => Subset,
            Within => Set (Asserts),
            Locus  => Programs.Locus (Luup, Subprogram (Asserts)));

      end if;

      return Count;

   end Loop_Start;


   function Loop_Count (
      Luup    : Loops.Loop_T;
      Asserts : Assertion_Map_T)
   return Flow.Execution.Bound_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Asserts.On_Loop(Loops.Index (Luup)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Own.Luup,
               Facts => +Loop_Repetitions));
      -- The assertions on the number of repetitions for this loop.

      Count : Flow.Execution.Bound_T := Flow.Execution.Unbounded;
      -- The intersection of all repetition-count assertions.

   begin

      Count_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Cumulator => Restrict_Count'Access,
         Param     => (null record),
         Total     => Count);

      if Flow.Execution.Void (Count) then

         Report_Conflict (
            Text   => "Conflicting assertions on loop repetitions",
            Subset => Subset,
            Within => Set (Asserts),
            Locus  => Programs.Locus (Luup, Subprogram (Asserts)));

      end if;

      return Count;

   end Loop_Count;


   function Call_Count (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Flow.Execution.Bound_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Asserts.On_Call(Programs.Index (Call)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Call_Static,
               Facts => +Call_Executions));
      -- The assertions on the number of executions of this call.

      Count : Flow.Execution.Bound_T := Flow.Execution.Unbounded;
      -- The intersection of all execution-count assertions.

   begin

      Count_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Cumulator => Restrict_Count'Access,
         Param     => (null record),
         Total     => Count);

      if Flow.Execution.Void (Count) then

         Report_Conflict (
            Text   => "Conflicting assertions on call executions",
            Subset => Subset,
            Within => Set (Asserts),
            Locus  => Programs.Locus (Call));

      end if;

      return Count;

   end Call_Count;


   function Call_Time (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Time_Bound_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Asserts.On_Call(Programs.Index (Call)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Call_Static,
               Facts => +Execution_Time));
      -- The assertions on the execution time of this call.

      Time : Time_Bound_T := No_Bound;
      -- The cumulative (intersected) time bounds.

   begin

      Execution_Time_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Cumulator => Restrict_Time'Access,
         Param     => (null record),
         Total     => Time);

      return Time;

   end Call_Time;


   function Call_Stack_Usage_Asserted (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Boolean
   is

      Usage : constant Stack_Bounds_T := Call_Stack_Usage (Call, Asserts);
       -- The asserted bounds on stack usage for this Call.

   begin

      for U in Usage'Range loop

         if Storage.Bounds.Known (Usage(U).Max) then
            -- This stack has an asserted finite usage.

            return True;

         end if;

      end loop;

      -- Zilch.

      return False;

   end Call_Stack_Usage_Asserted;


   function Call_Stack_Usage (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Stack_Bounds_T
   is

      Num_Stacks : constant Natural :=
         Programs.Number_Of_Stacks (Programs.Program (Call));
      -- The number of stacks in the program.

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Asserts.On_Call(Programs.Index (Call)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Call_Static,
               Facts => +Stack_Usage));
      -- The stack-usage assertions for this Call.

      Usage : Stack_Bounds_T (1 .. Num_Stacks) :=
         Default_Stack_Usage (Programs.Callee (Call));
      -- The cumulative (intersected) stack-usage bounds,
      -- initialized to the default (non-asserted) bounds.

   begin

      -- Asserted bounds on usage:

      Stack_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Cumulator => Restrict_Stacks'Access,
         Param     => (null record),
         Total     => Usage);

      return Usage;

   end Call_Stack_Usage;


   function Call_Final_Stack_Height_Asserted (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Boolean
   is
      use Storage.Bounds;

      Final : constant Stack_Bounds_T :=
         Call_Final_Stack_Height (Call, Asserts);
      -- The asserted bounds on final stack height for this Call.

      Stack : Programs.Stack_T;
      -- A stack, on which there may be assertions.

   begin

      for F in Final'Range loop

         Stack := Stack_By (Index => F, Map => Asserts);

         case Programs.Kind (Stack) is

         when Programs.Stable =>

            null;

         when Programs.Unstable =>

            if Known (Final(F)) then
               -- There are some asserted bounds on the final
               -- height of this stack.

               return True;

            end if;

         end case;

      end loop;

      -- Zilch.

      return False;

   end Call_Final_Stack_Height_Asserted;


   function Call_Final_Stack_Height (
      Call    : Programs.Call_T;
      Asserts : Assertion_Map_T)
   return Stack_Bounds_T
   is

      Num_Stacks : constant Natural :=
         Programs.Number_Of_Stacks (Programs.Program (Call));
      -- The number of stacks in the program.

      Subset : constant Assertion_Subset_T :=
         Part_Fact_Subsets.Subset (
            From   => Asserts.On_Call(Programs.Index (Call)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Fact'Access,
            Param  => (
               Part  => Call_Static,
               Facts => +Stack_Final));
      -- The final-stack-height assertions for this Call.

      Final : Stack_Bounds_T (1 .. Num_Stacks) :=
         Default_Final_Stack_Height (Programs.Program (Call));
      -- The cumulative (intersected) final stack height bounds,
      -- initialized to the default (non-asserted) bounds.

   begin

      Stack_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Cumulator => Restrict_Stacks'Access,
         Param     => (null record),
         Total     => Final);

      return Final;

   end Call_Final_Stack_Height;


   function Loop_Has_Property (
      Property : Processor.Property_T;
      Luup     : Loops.Loop_T;
      Asserts  : Assertion_Map_T)
   return Boolean
   is

      Subset : constant Assertion_Subset_T :=
         Part_Property_Subsets.Subset (
            From   => Asserts.On_Loop(Loops.Index (Luup)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Property'Access,
            Param  => (
               Part     => Own.Luup,
               Property => Property));
      -- The assertions on this Property for this Luup.

   begin

      return Subset'Length > 0;

   end Loop_Has_Property;


   function Call_Has_Property (
      Property : Processor.Property_T;
      Call     : Programs.Call_T;
      Asserts  : Assertion_Map_T)
   return Boolean
   is

      Subset : constant Assertion_Subset_T :=
         Part_Property_Subsets.Subset (
            From   => Asserts.On_Call(Programs.Index (Call)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Property'Access,
            Param  => (
               Part     => Call_Static,
               Property => Property));
      -- The assertions on this Property for this Call.

   begin

      return Subset'Length > 0;

   end Call_Has_Property;


   function Loop_Property (
      Property : Processor.Property_T;
      Luup     : Loops.Loop_T;
      Asserts  : Assertion_Map_T)
   return Storage.Bounds.Interval_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Property_Subsets.Subset (
            From   => Asserts.On_Loop(Loops.Index (Luup)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Property'Access,
            Param  => (
               Part     => Own.Luup,
               Property => Property));
      -- The assertions on this Property for this Luup.

      Interval : Storage.Bounds.Interval_T :=
         Storage.Bounds.Universal_Interval;
      -- The intersection of all bounds from the Subset.

   begin

      if Subset'Length = 0 then

         raise No_Such_Property;

      end if;

      Value_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Cumulator => Restrict_Property'Access,
         Param     => (null record),
         Total     => Interval);

      return Interval;

   end Loop_Property;


   function Call_Property (
      Property : Processor.Property_T;
      Call     : Programs.Call_T;
      Asserts  : Assertion_Map_T)
   return Storage.Bounds.Interval_T
   is

      Subset : constant Assertion_Subset_T :=
         Part_Property_Subsets.Subset (
            From   => Asserts.On_Call(Programs.Index (Call)).all,
            Within => Assertion_Bag_T (Asserts.Set.all),
            Filter => Match_Part_Property'Access,
            Param  => (
               Part     => Call_Static,
               Property => Property));
      -- The assertions on this Property for this Call.

      Interval : Storage.Bounds.Interval_T :=
         Storage.Bounds.Universal_Interval;
      -- The intersection of all bounds from the Subset.

   begin

      if Subset'Length = 0 then

         raise No_Such_Property;

      end if;

      Value_Bounds.Cumulate (
         From      => Subset,
         Within    => Assertion_Bag_T (Asserts.Set.all),
         Cumulator => Restrict_Property'Access,
         Param     => (null record),
         Total     => Interval);

      return Interval;

   end Call_Property;


   function Node_Property (
      Property : Processor.Property_T;
      Node     : Flow.Node_T;
      Asserts  : Assertion_Map_T)
   return Storage.Bounds.Interval_T
   is

      Table : constant Property_Table_Index_T :=
         Asserts.Properties (Flow.Index (Node));
      -- The index of the property-table for this node.

   begin

     return Asserts.Property_Tables (Table) (Property);

  end Node_Property;


end Assertions;
