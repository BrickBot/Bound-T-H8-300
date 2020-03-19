-- Programs (body)
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
-- $Revision: 1.94 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: programs.adb,v $
-- Revision 1.94  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.93  2014/06/30 19:10:32  niklas
-- Added function Locus (Step, Subprogram) for convenience.
--
-- Revision 1.92  2014/06/11 12:50:01  niklas
-- Added Add_Unstable_Stack_Without_Pointer, for OCL convenience.
--
-- Revision 1.91  2013-02-12 08:47:20  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.90  2011-10-18 20:19:49  niklas
-- Updated to provide the Subprogram as a parameter to
-- Processor.Properties.Entry_Bounds, an update required by
-- the ALF export function.
--
-- Revision 1.89  2011-09-08 14:14:06  niklas
-- Added Height_Unit for stacks.
--
-- Revision 1.88  2011-09-01 22:12:17  niklas
-- Added function Exists (Program_T).
--
-- Revision 1.87  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.86  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.85  2009-12-17 14:05:54  niklas
-- BT-CH-0197: Assertions on instruction roles.
--
-- Revision 1.84  2009-12-09 06:25:21  niklas
-- Added function Processor_Info to get the processor-specific
-- info for the Program_T that contains a given Subprogram_T.
--
-- Revision 1.83  2009-11-27 11:28:07  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.82  2009/04/16 16:29:24  niklas
-- BT-CH-0171: Stack pointers in call effects.
--
-- Revision 1.81  2008/09/07 07:29:09  niklas
-- BT-CH-0142: Conditional calls and option -[no_]tail_calls.
--
-- Revision 1.80  2008/07/23 09:07:17  niklas
-- BT-CH-0139: Fix recursion in Programs.Execution.Paths.
--
-- Revision 1.79  2008/07/20 06:31:39  niklas
-- BT-CH-0137: Cached Locus for subprograms and execution bounds.
--
-- Revision 1.78  2008/03/12 10:31:44  niklas
-- BT-CH-0122: Define_As_Root and other changes in Sub_Info_T.
--
-- Revision 1.77  2008/03/11 22:08:06  niklas
-- BT-CH-0121: Delayed calls and other SHARC support.
--
-- Revision 1.76  2008/02/15 20:27:43  niklas
-- BT-CH-0110: Better "&" for call-paths in Output loci.
--
-- Revision 1.75  2007/12/17 13:54:40  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.74  2007/10/18 13:14:51  niklas
-- Added Subprogram_Object_T.Root : Boolean to mark root subprograms.
-- Added function Root to show if a subprogram is a root.
-- Extended function New_Subprogram to make non-Roots by default.
-- Extended procedure Add_Root to mark the subprogram as a root.
--
-- Revision 1.73  2007/08/17 14:44:01  niklas
-- BT-CH-0074: Stable and Unstable stacks.
--
-- Revision 1.72  2007/08/16 07:09:38  niklas
-- Accepted merge of 1.63.2.1.
--
-- Revision 1.71  2007/08/06 09:20:04  niklas
-- Fixed trace message syntax.
--
-- Revision 1.70  2007/08/03 19:10:09  niklas
-- Added function Index_And_Name for tracing purposes.
-- Extended New_Subrogram to trace with Index_And_Name.
-- Added Notes to function Unused and to procedures Set_Unused
-- and Identify_Callee.
--
-- Revision 1.69  2007/07/09 13:34:35  niklas
-- Redesigned (corrected) function Subprograms (Program) to yield
-- all subprograms even when there is recursion among them.
--
-- Revision 1.68  2007/04/29 12:06:17  niklas
-- Extended function Name (Subprogram...) to use Opt.Qualify_Names.
--
-- Revision 1.67  2007/03/18 12:50:40  niklas
-- BT-CH-0050.
--
-- Revision 1.66  2007/03/09 13:48:31  niklas
-- Added Subprogram_Exists_At and Subprogram_At.
--
-- Revision 1.65  2007/03/05 11:06:46  niklas
-- BT-CH-0047.
--
-- Revision 1.64  2007/02/24 09:51:52  niklas
-- BT-CH-0046.
--
-- Revision 1.63.2.1  2007/06/06 20:04:07  niklas
-- Redesigned (corrected) function Subprograms (Program) to yield
-- all subprograms even when there is recursion among them.
--
-- Revision 1.63  2007/01/25 21:25:18  niklas
-- BT-CH-0043.
--
-- Revision 1.62  2007/01/21 19:31:48  niklas
-- BT-CH-0042.
--
-- Revision 1.61  2007/01/13 13:51:07  niklas
-- BT-CH-0041.
--
-- Revision 1.60  2006/11/26 22:07:27  niklas
-- BT-CH-0039.
--
-- Revision 1.59  2006/10/28 19:52:16  niklas
-- BT-CH-0031.
--
-- Revision 1.58  2006/08/22 13:50:48  niklas
-- BT-CH-0025.
--
-- Revision 1.57  2006/05/27 21:39:52  niklas
-- BT-CH-0020.
--
-- Revision 1.56  2006/05/17 20:06:34  niklas
-- Added the Take_Off_Node functions.
--
-- Revision 1.55  2006/05/06 06:59:22  niklas
-- BT-CH-0021.
--
-- Revision 1.54  2006/03/25 13:27:37  niklas
-- Added a "name" attribute to Program_T.
--
-- Revision 1.53  2006/02/27 20:08:11  niklas
-- Extended procedure Initialize to call Programs.Patching.
--
-- Revision 1.52  2005/09/23 10:51:06  niklas
-- Added function Number_Of_All_Calls.
--
-- Revision 1.51  2005/09/05 11:23:38  niklas
-- BT-CH-0007.
--
-- Revision 1.50  2005/09/03 11:50:30  niklas
-- BT-CH-0006.
--
-- Revision 1.49  2005/08/24 10:12:56  niklas
-- Added stuff to support the Analysis Workbench, including
-- the constant No_Calls, the inquiry functions Node (Call),
-- Nodes (Call_List), Non_Looped_Calls (Subprogram),
-- Containing_Loops (Call), and Calls_Within (Loop).
--
-- Revision 1.48  2005/06/12 07:30:25  niklas
-- Changed the display of calls from A->B to A=>B, to avoid
-- confusion with '-' used in approximate line-numbers.
--
-- Revision 1.47  2005/02/23 09:05:20  niklas
-- BT-CH-0005.
--
-- Revision 1.46  2005/02/20 15:15:37  niklas
-- BT-CH-0004.
--
-- Revision 1.45  2005/02/16 21:11:48  niklas
-- BT-CH-0002.
--
-- Revision 1.44  2005/02/04 20:50:12  niklas
-- Added the "Returns" attribute to subprograms, to show if the
-- subprogram can ever return to the caller. In some cases, marking
-- error-handling subprograms as "never returning" can change an
-- irreducible flow-graph to a reducible one, allowing analysis.
--
-- Revision 1.43  2005/01/02 22:32:19  niklas
-- Added entry address to note re missing subprogram symbol.
--
-- Revision 1.42  2004/05/01 08:44:18  niklas
-- First Tidorum version.
-- Taking Cell_T stuff from Storage, not Arithmetic.
-- Added Reducible attribute to Subprogram_T.
-- Defined public Subprogram_Index_T and Number_Of_Subprograms, to let
-- clients create arrays indexed by subprogram.
-- Renamed the function Calls to Calls_From to reduce ambiguity.
-- Added the analogous function Calls_To.
-- Explicit reference semantics for Subprogram_T means that all
-- such parameters are now mode "in" even if the underlying object
-- is modified.
-- The functions Locus and Entry_Locus take a Qualified option to
-- control the format of the identifiers.
-- Add the private operation Call_Arrow to help output formatting.
-- Added Trace output to some operations.
-- Find_Subprogram now reports multiple (ambiguous) matches when
-- such occur.
--
-- Revision 1.41  2003/03/11 08:28:02  holsti
-- Split execution-bounds stuff from the Programs package to
-- make the child package Programs.Execution.
-- Normalized some lay-out and commenting.
--
-- Revision 1.40  2002/11/30 10:43:18  holsti
-- Added some checks for undefined i/o cells.
--
-- Revision 1.39  2002/11/29 11:05:19  holsti
-- Added functions Root_Subprograms, Subprograms_Ascending and
-- Subprograms_Descending. Modified the function Subprograms to return
-- only the analyzed subprograms (those called from the roots) and to
-- exclude subprograms that exist only because of assertions.
-- This fixes NC_0146.
--
-- Revision 1.38  2001/12/10 12:58:51  holsti
-- Execution-bound objects are numbered (indexed) with a unique
-- sequential index, Execution_Bounds_Index_T. It is automatically
-- assigned by Initialize_Bounds, Bound_Time and Create_Blank_Bounds
-- (which take a Program parameter to update the bounds counter) and
-- accessed by the new function Index.
--
-- Renamed Max_Bounds to Number_Of_Bounds and added a synonymous
-- function to report the number of execution bounds in a program,
-- counting all subprograms.
--
-- Revision 1.37  2001/11/19 11:22:46  saarinen
-- Modified for Bound-T/ERC32:
-- Added function Root_Call.
-- Added function Lowest_WCET.
-- Added functions Max_Bounds and Bound_At.
-- Modified Processor.Sub_Info_T to Processor.Program.Sub_Info_T
-- Processor.Validate_Infos -> Processor.Program.Validate_Infos.
--
-- Revision 1.36  2001/05/20 13:16:17  holsti
-- Added Set_Processor_Info (NC_117).
--
-- Revision 1.35  2001/04/11 13:22:16  ville
-- Parameter Delimiter added to Programs.Identify
--
-- Revision 1.34  2001/04/09 10:10:30  ville
-- Updated to use renamed operation Subprogram_Connections
--
-- Revision 1.33  2001/04/04 10:22:03  holsti
-- Add_Root replaces Set_Roots and Make_Root_Call.
--
-- Revision 1.32  2001/03/26 14:02:10  holsti
-- Identify (by string) warns about multiple connections. Also, if
-- there are no connections for the string as identifier, tries to
-- use the string as a code address.
-- Identify (by address) uses only subprogram connections and warns
-- about multiple connections.
--
-- Revision 1.31  2001/03/21 20:29:22  holsti
-- Locus functions added. Other updates to new Output.
--
-- Revision 1.30  2001/03/15 20:50:13  holsti
-- Call_Bounds (No_Bounds) returns No_Bounds, not error.
--
-- Revision 1.29  2001/03/15 12:03:12  holsti
-- Node_Times optionally include lower-level calls.
--
-- Revision 1.28  2001/03/10 00:46:13  holsti
-- Extensive updates to the handling of calls and their effects.
-- Call_Step_T and Call_Step_Set_T removed.
-- Input and output cell-set attributes added to subprograms and
-- execution bounds.
-- Basis cell-set and initial cell-bounds attributes added to
-- execution bounds.
-- Parameter mapping implemented, using dynamic access.
-- A "launched" attribute is added to calls.
--
-- Revision 1.27  2001/03/06 09:36:48  ville
-- Root-calls excluded from returned calls
--
-- Revision 1.26  2001/02/19 14:49:36  holsti
-- Store_Bounds made public, not always automatic.
--
-- Revision 1.25  2001/02/19 09:29:35  holsti
-- Execution bounds are identified by call path and stored with
-- the subprogram to which they apply. No execution bounds are
-- stored with specific calls. Detailed changes include:
-- Execution_Bounds_T renamed to Execution_Bounds_Ref.
-- Loop bounds with execution bounds no longer contain a copy
-- of the loops of the subprogram, just the bounds; there is
-- no risk of "loop not found".
-- Call_Path_T added.
-- Initialize_Bounds replaces the Erase procedure.
-- Bound_Time replaces the Bound_WCET procedure.
-- All Is_Call_Dependent functions deleted.
-- New_Subprogram encapsulates the creation of new subprograms.
-- Call_Bounds moved to logical position.
-- Functions to get the subprogram and call-path from a set of
-- execution bounds added.
-- WCET_Bounded renamed to Time_Bounded.
-- WCET renamed to Time.
-- Stack_Bounded added, checks if execution bounds include stack.
-- All functions that get execution-bound values are careful to
-- check for null bounds and return without exception.
-- Show_Stack_Path and Show_Stack use call-path-specific bounds.
--
-- Revision 1.24  2001/01/19 08:52:03  saarinen
-- NC_086: Subprograms in assertion file are left untraced.
--
-- Revision 1.23  2000/12/28 17:46:29  holsti
-- Some_Calls added.
--
-- Revision 1.22  2000/12/28 12:33:45  holsti
-- Subprogram_Name_List_T and Not_Implemented removed (not used).
-- Identify takes the Identifier as a String (not Unbounded_String).
-- Identify does not report "already mentioned" when the subprogram exists,
-- because this was relevant only for root subprograms on the command-line.
-- No_Subprogram added.
--
-- Revision 1.21  2000/12/22 11:20:45  saarinen
-- Fixed NC_025, NC_026 and NC_028.
-- Changed Node_Times in Execution_Bounds_Object to be a reference.
--
-- Revision 1.20  2000/12/21 14:39:23  sihvo
-- Added No_Bounds.
--
-- Revision 1.19  2000/12/21 08:42:27  ville
-- Correction for NC_027
--
-- Revision 1.18  2000/12/21 08:20:43  sihvo
-- Minor changes in layout etc.
--
-- Revision 1.17  2000/12/05 15:43:46  holsti
-- The term "loop neck" replaces the overloaded "loop entry".
-- Decoder.Stack_Height_Cell replaces deleted Arithmetic function.
--
-- Revision 1.16  2000/11/24 12:06:01  sihvo
-- Added stack height analysis.
--
-- Revision 1.15  2000/11/24 10:15:47  saarinen
-- Added attribute Var_Cells into Subprogram_Object_T.
-- Added functions to manipulate Var_Cells.
-- Deleted Set_WCET.
--
-- Revision 1.14  2000/11/22 22:32:31  holsti
-- Using Programs.Code_Address_T instead of Programs.Address_T.
-- Added function Calls (From : Subprogram_T; Into : Subprogram_Set_T).
--
-- Revision 1.13  2000/11/09 13:41:49  saarinen
-- Fixed WCET_Done and Is_Call_Dependent.
--
-- Revision 1.12  2000/11/03 10:09:14  saarinen
-- Fixed Find_Subprogram (NC_019).
--
-- Revision 1.11  2000/10/26 10:13:31  saarinen
-- Added fuction Call_Bounds.
-- Modified Set_Bounds so that bounds can be set to empty.
--
-- Revision 1.10  2000/10/06 13:58:16  saarinen
-- Fixed Calls_Between to handle root calls.
--
-- Revision 1.9  2000/09/20 19:32:06  saarinen
-- Added implemention to many functions and removed some unused ones.
--
-- Revision 1.8  2000/08/18 18:07:53  holsti
-- Unbounded_Vectors Index_Type removed.
--
-- Revision 1.7  2000/08/11 12:59:28  saarinen
-- Added some attributes into Subprogram_Object and Call_Object.
-- Implemented some subprograms.
--
-- Revision 1.6  2000/08/04 08:31:15  saarinen
-- Added stubs for many subprograms.
--
-- Revision 1.5  2000/07/13 11:31:01  saarinen
-- Added Call_Step_Set_T, modified Program_T, added support for
-- Call_Graph creation and made some other minor changes.
--
-- Revision 1.4  2000/07/04 10:23:15  parviain
-- added functions to acceess call infomation
--
-- Revision 1.3  2000/05/05 12:16:51  holsti
-- Procedure Identify implemented & other changes.
--
-- Revision 1.2  2000/04/27 10:46:34  holsti
-- First implementation.
--


with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Bags;   -- From MW Components.

with Decoder;
with File_System;
with Flow.Show;
with Loops.Show;
with Options.Command;
with Output;
with Processor.Properties;
with Programs.Opt;
with Programs.Patching;
with Programs.Sort;
with Storage.Bounds;
with String_Pool;
with Symbols;
with Symbols.Show;
with Unbounded_Vectors;


package body Programs is


   Empty_Subprogram_List : Subprogram_List_T (1 .. 0);
   --
   -- To be returned for an empty subprogram set.


   --
   ---   Call_Set_T
   --


   package Call_Vectors is new Unbounded_Vectors (
      Element_Type   => Call_T,
      Vector_Type    => Call_List_T,
      Initial_Size   => 10,
      Size_Increment => 10,
      Deallocate     => Opt.Deallocate);

   subtype Call_Set_T is Call_Vectors.Unbounded_Vector;


   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;
   --
   -- For subprogram names.


   --
   ---   Subprograms and the set of subprograms
   --


   type Subprogram_Object_T is record

      Program         : Program_T;
      Scope           : Symbols.Scope_T;
      Name            : Unbounded_String;
      Index           : Subprogram_Index_T;

      Root            : Boolean;
      Stub            : Boolean;

      Entry_Address   : Processor.Code_Address_T;
      Flow_Graph      : Flow.Graph_T;
      Processor_Info  : Processor.Program.Sub_Info_T;

      Locus           : Output.Locus_T;
      Locus_Max_Step  : Flow.Step_Count_T;

      Calls           : Call_Set_T;
      Reducible       : Boolean;
      Loop_Info       : Loops.Loops_Ref;
      Called          : Boolean;
      Unused          : Boolean;
      Arith_Choice    : Arithmetic.Opt.Possible_Choice_T;
      Return_Method   : Calling.Return_Method_T;
      Integrate_Calls : Boolean;
      Hide_In_Drawing : Boolean;

   end record;
   --
   -- A subprogram with attributes as follows:
   --
   -- Program
   --    The program of which the subprogram is a part.
   --
   -- Scope
   --    The scope of the name (symbol) of the subprogram.
   --    Contents are target-dependent but usually include
   --    the source-file name or module name.
   --
   -- Name
   --    The simple name (identifier) of the subprogram.
   --    Depending on the programming language and the compiler
   --    used for the target program, this may contain prefixes that
   --    identify the module, although the module may also be shown
   --    in the Scope.
   --
   -- Index
   --    The identifying sequential index 1 .. of this subprogram
   --    object. The index is unique within the specific Program_T
   --    in which this subprogram object resides.
   --
   -- Root
   --    Whether this subprogram is a root subprogram, that is, the
   --    root of a call-graph to be analysed.
   --
   -- Stub
   --    Whether this subprogram is a stub. If so, the subprogram is
   --    not really analysed; the Flow_Graph (if any) is a dummy; the
   --    arithmetic effect of the subprogram is unknown, and the
   --    resource bounds (if any) must be asserted.
   --
   -- Locus
   --    The last computed Output.Locus_T for the Flow_Graph.
   --    This is a "cached" value, recomputed only when the Flow_Graph
   --    has changed since the Locus was last computed.
   --
   -- Locus_Max_Step
   --    The value of Flow.Max_Step (Flow_Graph) captured when the
   --    Locus was last computed. The Flow_Graph is considered to have
   --    changed, triggering a new Locus computation, if the current
   --    Max_Step (Flow_Graph) is larger than Locus_Max_Step.
   --
   -- Called
   --    Whether a call to this subprogram has been found (so
   --    the subprogram is included in the analysis unless it is
   --    considered to be Unused). Root subprograms are not automatically
   --    considered to be called, but they are included in the analysis
   --    anyway. If Called is False for a non-root subprogram, the
   --    subprogram is just mentioned in the assertion file, but no
   --    calls have yet been found.
   --
   -- Unused
   --    Whether this subprogram is known (or assumed or asserted) to
   --    be unused in any execution under analysis. This is usually the
   --    result of an explicit assertion on this subprogram. An unused
   --    subprogram is not analysed at all and all calls to it are
   --    considered to be infeasible.
   --
   -- Arith_Choice
   --    Whether arithmetic analysis is allowed, denied, or forced for
   --    this subprogram.
   --
   -- Return_Method
   --    If and how the subprogram returns to its caller.
   --
   -- Integrate_Calls
   --    Whether calls to the subprogram should be decoded in the
   --    "integrated" way and become part of the caller's flow-graph.
   --
   -- Hide_In_Drawing
   --    Whether this subprogram, and all calls to and from it, should
   --    be hidden (omitted) from the drawings of the call graph and
   --    bounds graph.


   package Subprogram_Bags is new Bags (
      Key_Type  => Processor.Code_Address_T,
      Item_Type => Subprogram_T,
      Key_Of    => Entry_Address,
      "<"       => Processor."<",  -- for Code_Address_T
      "="       => Processor."=",  -- for Code_Address_T
      Count     => Natural);


   subtype Subprog_Set_T is
      Subprogram_Bags.Bag (Duplicate_Keys_Allowed => False);
   --
   -- A set of subprograms, keyed on Entry_Address.


   type Subprogram_Set_Object_T is new Subprog_Set_T;
   --
   -- A "new" is required to implement the private type promised by
   -- the package declaration.


   --
   ---   Calling contexts = call paths
   --


   function Null_Call_Path return Call_Path_T
   is
   begin

      return (1 ..0 => null);

   end Null_Call_Path;


   function "<=" (Left, Right : Call_Path_T) return Boolean
   is
   begin

      return Left'Length <= Right'Length
      and then
         Left = Right (Right'Last - Left'Length + 1 .. Right'Last);

   end "<=";


   function "-" (Left, Right : Call_Path_T) return Call_Path_T
   is
   begin

      if Right <= Left then

         return Left (Left'First .. Left'Length - Right'Length);

      else

         Output.Fault (
            Location => "Programs.""-"" (Call_Path_T)",
            Text     => "Right is not a suffix of Left");

         raise Constraint_Error;

      end if;

   end "-";


   function Image (Path : Call_Path_T)
   return String
   is

      function Tail_Image (Tail : Call_Path_T)
      return String
      --
      -- Return the path image, starting with the callee of
      -- the first call.
      --
      is
      begin

         if Tail'Length = 0 then

            return "";

         else

            return
                 Call_Arrow (Tail(Tail'First))
               & Tail_Image (Tail(Tail'First + 1 .. Tail'Last));

         end if;

      end Tail_Image;

   begin  -- Image

      if Path'Length = 0 then

         return "[]";

      elsif Caller (Path(Path'First)) = null then
         -- The path starts with a root call.

         return
              Name (Callee (Path(Path'First)))
            & Tail_Image (Path(Path'First + 1 .. Path'Last));

      else

         return
              Name (Caller (Path(Path'First)))
            & Tail_Image (Path);

      end if;

   end Image;


   function Locus (Path : Call_Path_T)
   return Output.Locus_T
   is
      use type Output.Locus_T;
   begin

      if Path'Length = 0 then

         return Output.Locus (Call_Path => Image (Path));

      else

         return Output.Locus (Call_Path => Image (Path))
              & Locus (Callee (Path(Path'Last)));

      end if;

   end Locus;


   --
   ---   Stacks
   --


   function Name (Stack : Stack_T) return String
   is
   begin

      return Stack.Name.all;

   end Name;


   function Kind (Stack : Stack_T) return Stack_Kind_T
   is
   begin

      return Stack.Kind;

   end Kind;


   function Height (Stack : Stack_T) return Storage.Cell_T
   is
   begin

      return Stack.Height_Cell;

   end Height;


   function Height (Stack : Stack_T) return Arithmetic.Variable_T
   is
   begin

      return Stack.Height_Var;

   end Height;


   function Height_Unit (Stack : Stack_T)
   return Arithmetic.Positive_Value_T
   is
   begin

      return Stack.Height_Unit;

   end Height_Unit;


   function Pointer (Stack : Stack_T) return Storage.Cell_T
   is
   begin

      return Stack.Pointer_Cell;

   end Pointer;


   function Pointer (Stack : Stack_T) return Arithmetic.Variable_T
   is
   begin

      return Stack.Pointer_Var;

   end Pointer;


   function Coupling (Stack : Stack_T) return Arithmetic.Algebra.Coupling_T
   is
   begin

      return Stack.Coupling;

   end Coupling;

   function Width_Of (Stack : Stack_T) return Arithmetic.Width_T
   is
   begin

      return Storage.Width_Of (Stack.Height_Cell);

   end Width_Of;


   function Net_Change (Stack : Stack_T) return Storage.Bounds.Limit_T
   is
   begin

      return Stack.Net_Change;

   end Net_Change;


   function Index (Stack : Stack_T) return Stack_Index_T
   is
   begin

      return Stack.Index;

   end Index;


   package Stack_Vectors is new Unbounded_Vectors (
      Element_Type   => Stack_T,
      Vector_Type    => Stacks_T,
      Initial_Size   => 3,
      Size_Increment => 10,
      Deallocate     => Opt.Deallocate);

   type Stack_List_T is new Stack_Vectors.Unbounded_Vector;
   --
   -- For use in Program_Object_T, below.


   --
   ---   Compilers
   --


   procedure Set_Option (
      Argument : in     String;
      Compiler : in out Compiler_T;
      Valid    :    out Boolean)
   is
   begin

      Valid := False;

   end Set_Option;


   procedure Set_Warn_Option (
      Item     : in      String;
      Compiler : in out Compiler_T;
      Valid    :    out Boolean)
   is
   begin

      Valid := False;

   end Set_Warn_Option;


   procedure List_Options (Compiler : in Compiler_T)
   is
   begin

      Ada.Text_IO.Put_Line (
           "Options for the "
         & Name (Compiler_T'Class (Compiler))
         & " compiler: none.");

      Ada.Text_IO.New_Line;

   end List_Options;


   procedure List_Warn_Options (Compiler : in Compiler_T)
   is
   begin

      null;

   end List_Warn_Options;


   --
   --    A null compiler
   --


   function Name (Item : Null_Compiler_T) return String
   is
   begin

      return "null";

   end Name;


   --
   --    Roles of instructions
   --


   type Role_Point_T is record
      Address : Processor.Code_Address_T;
      Role    : Processor.Instruction_Role_T;
      Used    : Boolean;
   end record;
   --
   -- One point in the mapping from code address to the role
   -- assigned to the instruction residing at that address.
   -- The Used component shows whether the point has been used
   -- (returned by Instruction_Role).


   function Address_Of (Item : Role_Point_T)
   return Processor.Code_Address_T
   is
   begin

      return Item.Address;

   end Address_Of;


   package Role_Maps is new Bags (
      Key_Type  => Processor.Code_Address_T,
      Item_Type => Role_Point_T,
      Key_Of    => Address_Of,
      "<"       => Processor."<",  -- for Code_Address_T
      "="       => Processor."=",  -- for Code_Address_T
      Count     => Natural);


   subtype Role_Map_T is
      Role_Maps.Bag (Duplicate_Keys_Allowed => False);
   --
   -- A partial mapping from Code Address to Instruction Role.


   --
   --    Programs:
   --


   type Program_Object_T is record
      Name              : String_Pool.Item_T;
      Processor_Info    : Processor.Program.Info_T;
      Symbol_Table      : Symbols.Symbol_Table_T;
      Subprogram_Count  : Subprogram_Count_T := 0;
      Subprograms       : Subprog_Set_T;
      Roots             : Call_Set_T;
      Calls             : Call_Set_T;
      Shoots            : Subprogram_Set_T;
      Stacks            : Stack_List_T;
      Compiler          : Compiler_Ref;
      Instruction_Roles : Role_Map_T;
   end record;
   --
   -- A program with attributes as follows:
   --
   -- Name
   --    A readable name for the program.
   --    No logical meaning.
   -- Processor_Info
   --    Processor-specific information for this program. Usually
   --    contains handles through which one can access the memory
   --    image as loaded from the executable binary file. May contain
   --    other information to help analyse this program.
   -- Symbol_Table
   --    Connections between machine-level and source-code-level
   --    entities in this program, as far as defined in the executable
   --    file. Usually contains symbols (identifiers) for subprograms
   --    and variables, connected to their memory addresses, and names
   --    of source-code files with line-numbers connected to code
   --    addresses.
   -- Subprogram_Count
   --    The number of subprograms created within this program.
   -- Subprograms
   --    The set of subprograms created within this program, containing
   --    Subprogram_Count elements, indexed 1 .. Subprogram_Count.
   -- Roots
   --    The "root" calls from which the analysis starts.
   --    These contain all the maximal (topmost) subprograms in the
   --    analysed subprogram hierarchy formed by the call relationship.
   --    However, some root subprograms may also be called by other
   --    subprograms (root or non-root), although this means that listing
   --    them as roots is redundant and does not increase the set of
   --    subprograms analysed.
   -- Calls
   --    All the calls between analysed subprograms.
   -- Shoots
   --    The set of "shoot" subprograms. Whenever a non-root call is added
   --    to the program and this is the first call to the callee, this
   --    callee is added to the Shoots set. A client can collect and
   --    erase the Shoots set at any time with the procedure Move_Shoots,
   --    which see.
   -- Stacks
   --    All the stacks the Decoder has defined for this program.
   -- Compiler
   --    The compiler that generated this program.
   -- Roles
   --    Special roles assigned (asserted) for some instructions
   --    in this program.


   --
   --    Calls
   --


   type Call_Object_T is record
      Caller    : Subprogram_T;
      Callee    : Subprogram_T;
      Index     : Call_Index_T;
      Flow_Step : Flow.Step_T;
      Protocol  : Calling.Protocol_Ref;
   end record;
   --
   -- A call, with attributes as follows:
   --
   -- Caller
   --    The calling (higher-level) subprogram.
   --
   -- Callee
   --    The called (lower-level) subprogram.
   --
   -- Index
   --    The unique sequential index of this call among all the
   --    calls from the Callee.
   --
   -- Flow_Step
   --    The step in the caller's flow-graph that contains (or
   --    is) the call.
   --
   -- Protocol
   --    Refers to the (instance of) the primitive calling protocol
   --    used for this call, as assigned in the decoding phase.
   --    Later analysis phases can start from this to create refined
   --    or altered protocol objects for the call as it occurs in
   --    refined computation models.


   function Exists (Program : Program_T)
   return Boolean
   is
   begin

      return Program /= null;

   end Exists;


   procedure Initialize (
      From_File : in     String;
      Program   :    out Program_T)
   is

      Patches_Valid : Boolean;
      -- The patches, if any, were successfully applied.

   begin

      -- Initialize the private structures:

      Program := new Program_Object_T;

      Program.Name := String_Pool.To_Item (
         File_System.File_Name (From_File));

      Subprogram_Bags.Destroy (Program.Subprograms);
      -- Erases the subprogram set.

      -- The sets of Roots, Calls and Shoots are empty by default.

      Symbols.Erase (Program.Symbol_Table);

      -- Load the program from the executable file:

      Decoder.Initialize (
         File_Name => From_File,
         Program   => Program);

      -- Apply patches if any:

      Programs.Patching.Apply_Patches (
         Program => Program,
         Valid   => Patches_Valid);

      if not Patches_Valid then

         raise Options.Command.Argument_Error;

      end if;

   end Initialize;


   function Name (Program : Program_T) return String
   is
   begin

      return String_Pool.To_String (Program.Name);

   end Name;


   procedure Set_Processor_Info (
      To     : in Processor.Program.Info_T;
      Within : in Program_T)
   is
   begin

      Within.Processor_Info := To;

   end Set_Processor_Info;


   function Processor_Info (Program : Program_T)
   return Processor.Program.Info_T
   is
   begin

      return Program.Processor_Info;

   end Processor_Info;


   function Symbol_Table (Program : Program_T)
   return Symbols.Symbol_Table_T
   is
   begin

      return Program.Symbol_Table;

   end Symbol_Table;


   function Number_Of_Subprograms (Within : Program_T)
   return Subprogram_Count_T
   is
   begin

      return Within.Subprogram_Count;

   end Number_Of_Subprograms;


   function Subprogram_Entries (
      Name   : String;
      Within : Program_T)
   return Processor.Code_Address_List_T
   is

      Scope : Symbols.Scope_T := Symbols.Global_Scope;
      -- We have no scope info.

      Conns : constant Symbols.Connection_Set_T :=
         Symbols.Subprogram_Connections (
            Scope  => Scope,
            Name   => Name,
            Within => Symbol_Table (Within));
      -- All subprograms connected to this Name in the global Scope.

      Entries : Processor.Code_Address_List_T (Conns'Range);
      -- The address of the connections.

   begin

      for E in Entries'Range loop

         Entries(E) := Symbols.Address_Of (Conns(E));

      end loop;

      return Entries;

   end Subprogram_Entries;


   function New_Subprogram (
      Scope   : Symbols.Scope_T;
      Name    : String;
      Address : Processor.Code_Address_T;
      Info    : Processor.Program.Sub_Info_T;
      Within  : Program_T)
   return Subprogram_T
   --
   -- Creates a new subprogram with the given attributes, and
   -- other attributes with initial or null values. Enters the
   -- new subprogram Within the given program and applies any
   -- target-specific initialization.
   --
   is
      use Ada.Strings.Unbounded;

      Sub : Subprogram_T := new Subprogram_Object_T;
      -- The new subprogram.

   begin

      -- Count the subprogram:

      Within.Subprogram_Count := Within.Subprogram_Count + 1;

      -- An aggregate is not used to initialise Sub, because some
      -- components are private and have default initial values for
      -- which there are no assignable expressions.

      Sub.Program         := Within;
      Sub.Scope           := Scope;
      Sub.Name            := To_Unbounded_String (Name);
      Sub.Index           := Within.Subprogram_Count;

      Sub.Root            := False;
      Sub.Stub            := False;

      Sub.Entry_Address   := Address;

      Flow.Create (Sub.Flow_Graph);

      Sub.Processor_Info  := Info;

      Sub.Locus           := Output.Locus (Statements =>
         Symbols.Show.Statements (
            Address => Address,
            Source  => Symbol_Table (Within)));

      Sub.Locus_Max_Step  := Flow.Max_Step (Sub.Flow_Graph);

      Sub.Reducible       := False;
      Sub.Called          := False;
      Sub.Unused          := False;
      Sub.Arith_Choice    := Arithmetic.Opt.Undefined;
      Sub.Return_Method   := (Way => Calling.Normal_Return);
      Sub.Integrate_Calls := False;
      Sub.Hide_In_Drawing := False;

      -- The following components are initialized by default:
      --
      -- Component     Default initial value
      -- ---------     ---------------------
      -- Calls         Empty set.
      -- Loops_Info    Null.

      if Opt.Trace_Subprograms then

        Output.Trace (
           Locus => Locus (Subprogram => Sub, Qualified => True),
           Text  => "Created subprogram " & Index_And_Name (Sub));

      end if;

      Subprogram_Bags.Insert (
         Item => Sub,
         Into => Within.Subprograms);

      Decoder.Initialize (
         Subprogram => Sub,
         Within     => Within);

      return Sub;

   end New_Subprogram;


   procedure Identify (
      Identifier : in    String;
      Delimiter  : in    Character;
      Program    : in    Program_T;
      Subprogram :    out Subprogram_T)
   is

      Conns : constant Symbols.Connection_Set_T :=
         Symbols.Subprogram_Connections (
            Scope  => Symbols.Scope_Of (Identifier, Delimiter),
            Name   => Symbols.Name_Of  (Identifier, Delimiter),
            Within => Program.Symbol_Table);
      -- All connections for the given symbolic identifier.

      Conn : Symbols.Connection_T;
      -- The chosen connection, if there are some.

      Entry_Address : Processor.Code_Address_T;
      -- The entry address of the subprogram.

   begin

      if Conns'Length > 0 then

         -- Use the first (or only) connection:

         Conn := Conns(Conns'First);

         Entry_Address := Symbols.Address_Of (Conn);

         -- Report the other (ambiguous) connections, if any:

         for C in Conns'First + 1 .. Conns'Last loop

            Output.Warning (
               Locus => Symbols.Show.Locus (
                  Connection => Conns(C),
                  Source     => Program.Symbol_Table),
               Text => "Other identifier connection not used.");

         end loop;

         if Conns'Length > 1 then

            Output.Warning (
               Locus => Symbols.Show.Locus (
                  Connection => Conn,
                  Source     => Program.Symbol_Table),
               Text => "This identifier connection used.");

         end if;

         -- Identify the subprogram by means of the entry address:

         if Subprogram_Bags.Member (
             Key    => Entry_Address,
             Of_Bag => Program.Subprograms)
         then

            Subprogram := Subprogram_Bags.Search (
               Key    => Entry_Address,
               Within => Program.Subprograms);

         else

            Subprogram :=
               New_Subprogram (
                  Scope   => Symbols.Scope_Of (Conn),
                  Name    => Symbols.Name_Of  (Conn),
                  Address => Entry_Address,
                  Info    => Processor.Properties.Blank_Info (
                     Address => Entry_Address,
                     Program => Program),
                  Within  => Program);

         end if;

      else
         -- No connections with this identifier.
         -- Perhaps it is a code address?

         Entry_Address := Processor.Properties.Subprogram_Address (Identifier);
         -- Raises Address_Error if string is not a valid address.

         -- The string could be interpreted as an address.
         -- Try to identify the subprogram with this address:

         Identify (
            Address    => Entry_Address,
            Program    => Program,
            Subprogram => Subprogram);

      end if;

   exception

   when Processor.Properties.Address_Error =>
      -- From Processor.Properties.Subprogram_Address.

      raise Subprogram_Not_Found;

   end Identify;


   procedure Identify (
      Address    : in     Processor.Code_Address_T;
      Program    : in     Program_T;
      Subprogram :    out Subprogram_T)
   is
      use type Symbols.Connection_Kind_T;

      Conns : constant Symbols.Connection_Set_T :=
         Symbols.Connections_For_Address (
            Address => Address,
            Within  => Program.Symbol_Table);
      -- All connections for the given code address.

      Connected : Natural := 0;
      -- Number of subprogram connections for this address.

      Conn : Symbols.Connection_T;
      -- The chosen connection, if there are some.

   begin

      if Subprogram_Bags.Member (
         Key    => Address,
         Of_Bag => Program.Subprograms)
      then
         -- This subprogram is already known.
         -- Never mind about the connections, if any.

         Subprogram := Subprogram_Bags.Search (
            Key    => Address,
            Within => Program.Subprograms);

      else
         -- Subprogram not known, but some connections
         -- with this address may be known.

         for C in Conns'Range loop

            if Symbols.Kind_Of (Conns(C)) = Symbols.Subprogram then

               Connected := Connected + 1;

               if Connected = 1 then
                  Conn := Conns(C);
               else
                  Output.Warning (
                     Locus => Symbols.Show.Locus (
                        Connection => Conns(C),
                        Source     => Program.Symbol_Table),
                     Text => "Other address connection not used.");
               end if;

            end if;

         end loop;

         if Connected > 1 then

            Output.Warning (
               Locus => Symbols.Show.Locus (
                  Connection => Conn,
                  Source     => Program.Symbol_Table),
               Text => "This address connection used.");

         end if;

         -- Are there any subprograms at this address?

         if Connected > 0 then
            -- Create the subprogram from the chosen connection.

            Subprogram :=
               New_Subprogram (
                  Scope   => Symbols.Scope_Of (Conn),
                  Name    => Symbols.Name_Of  (Conn),
                  Address => Address,
                  Info    => Processor.Properties.Blank_Info (
                     Address => Address,
                     Program => Program),
                  Within  => Program);

         else
            -- Create the subprogram from just the address.

            Subprogram :=
               New_Subprogram (
                  Scope   => Symbols.Scope_Of (""),
                  Name    => Processor.Image (Address),
                  Address => Address,
                  Info    => Processor.Properties.Blank_Info (
                     Address => Address,
                     Program => Program),
                  Within  => Program);

         end if;

      end if;

   end Identify;


   function Enters_Subprogram (
      Address : Processor.Code_Address_T;
      Program : Program_T)
   return Boolean
   is
      use type Symbols.Connection_Kind_T;
   begin

      if Subprogram_Bags.Member (
         Key    => Address,
         Of_Bag => Program.Subprograms)
      then
         -- This subprogram is already known.
         -- Never mind about the connections, if any.

         return True;

      else
         -- Subprogram not known, but some connections
         -- with this address may be known.

         declare

            Conns : constant Symbols.Connection_Set_T :=
               Symbols.Connections_For_Address (
                  Address => Address,
                  Within  => Program.Symbol_Table);
            -- All connections for the given code address.

         begin

            for C in Conns'Range loop

               if Symbols.Kind_Of (Conns(C)) = Symbols.Subprogram then

                  return True;

               end if;

            end loop;

            -- No Subprogram connections for this Address.

            return False;

         end;

      end if;

   end Enters_Subprogram;


   function Subprogram_Exists_At (
      Address : Processor.Code_Address_T;
      Within  : Program_T)
   return Boolean
   is
   begin

      return Subprogram_Bags.Member (
         Key    => Address,
         Of_Bag => Within.Subprograms);

   end Subprogram_Exists_At;


   function Subprogram_At (
      Address : Processor.Code_Address_T;
      Within  : Program_T)
   return Subprogram_T
   is
   begin

      return Subprogram_Bags.Search (
         Key    => Address,
         Within => Within.Subprograms);

   exception

   when Subprogram_Bags.Nonexistent_Key =>

      return No_Subprogram;

   end Subprogram_At;


   function Subprograms (Program : Program_T)
   return Subprogram_List_T
   is
   begin

      return Subprogram_List_T (Subprogram_Bags.To_List (Program.Subprograms));

   end Subprograms;


   function Subprograms_Ascending (Program : Program_T)
   return Subprogram_List_T
   is
   begin

      return
         Sort.Bottom_Up (
           Elements => Root_Subprograms (Program),
           Pairs    => Calls (Program));

   end Subprograms_Ascending;


   function Subprograms_Descending (Program : Program_T)
   return Subprogram_List_T
   is
   begin

      return
         Sort.Top_Down (
           Elements => Root_Subprograms (Program),
           Pairs    => Calls (Program));

   end Subprograms_Descending;


   function Calls (Program : Program_T) return Call_List_T
   is

      All_Calls : constant Call_List_T :=
         Call_Vectors.To_Vector (Program.Calls);
      -- All the calls, including root calls too.

      Real_Calls : Call_List_T (1 .. All_Calls'Length);
      Last : Natural := 0;
      -- The real (non-root) calls are Real_Calls(1..Last).

   begin

      for A in All_Calls'Range loop

         if All_Calls(A).Caller /= null then
            -- This is a real call, not a root-call.

            Last := Last + 1;

            Real_Calls(Last) := All_Calls(A);

         end if;

      end loop;

      return Real_Calls(1..Last);

   end Calls;


   function All_Calls (Program : Program_T) return Call_List_T
   is
   begin

      return Call_Vectors.To_Vector (Program.Calls);

   end All_Calls;


   function Number_Of_All_Calls (Program : Program_T) return Natural
   is
   begin

      return Call_Vectors.Length (Program.Calls);

   end Number_Of_All_Calls;


   procedure Move_Shoots (
      From : in     Program_T;
      To   : in out Subprogram_Set_T)
   is
   begin

      Add (
         To     => To,
         Adding => From.Shoots);

      Erase (From.Shoots);

   end Move_Shoots;


   procedure Add_Stable_Stack (
      Name     : in String;
      Pointer  : in Storage.Cell_T;
      Height   : in Storage.Cell_T;
      Unit     : in Arithmetic.Positive_Value_T;
      Coupling : in Arithmetic.Algebra.Coupling_T;
      To       : in Program_T)
   is
      use type Storage.Cell_T;

      Pointer_Var : Arithmetic.Variable_T;
      -- The Pointer as a variable, or Unknown if none.

   begin

      if Pointer = Storage.No_Cell then

         Pointer_Var := Arithmetic.Unknown;

      else

         Pointer_Var := Arithmetic.Cell (Pointer);

         if Storage.Is_Volatile (Pointer) then

            Output.Error (
                 "Stack pointer is volatile"
               & Output.Field_Separator
               & Name
               & Output.Field_Separator
               & Storage.Image (Pointer));

         end if;

      end if;

      if Storage.Is_Volatile (Height) then

         Output.Error (
              "Stack height is volatile"
            & Output.Field_Separator
            & Name
            & Output.Field_Separator
            & Storage.Image (Height));

      end if;

      Append (
         To    => To.Stacks,
         Value => Stack_T'(
            Kind         => Stable,
            Name         => new String'(Name),
            Height_Unit  => Unit,
            Pointer_Cell => Pointer,
            Pointer_Var  => Pointer_Var,
            Coupling     => Coupling,
            Height_Cell  => Height,
            Height_Var   => Arithmetic.Cell (Height),
            Net_Change   => Storage.Bounds.Limit (0),
            Index        => Next (To.Stacks)));

      Output.Note (
           "Added stable stack """
         & Name
         & """, pointer "
         & Arithmetic.Image (Pointer_Var)
         & " coupling by "
         & Arithmetic.Algebra.Coupling_T'Image (Coupling)
         & " to height "
         & Storage.Image (Height)
         & ", unit "
         & Arithmetic.Image (Unit)
         & " bits.");

   end Add_Stable_Stack;


   procedure Add_Unstable_Stack (
      Name       : in String;
      Pointer    : in Storage.Cell_T;
      Height     : in Storage.Cell_T;
      Unit       : in Arithmetic.Positive_Value_T;
      Coupling   : in Arithmetic.Algebra.Coupling_T;
      Net_Change : in Storage.Bounds.Limit_T;
      To         : in Program_T)
   is
      use type Storage.Cell_T;

      Pointer_Var : Arithmetic.Variable_T;
      -- The Pointer as a variable, or Unknown if none.

   begin

      if Pointer = Storage.No_Cell then

         Pointer_Var := Arithmetic.Unknown;

      else

         Pointer_Var := Arithmetic.Cell (Pointer);

         if Storage.Is_Volatile (Pointer) then

            Output.Error (
                 "Stack pointer is volatile"
               & Output.Field_Separator
               & Name
               & Output.Field_Separator
               & Storage.Image (Pointer));

         end if;

      end if;

      if Storage.Is_Volatile (Height) then

         Output.Error (
              "Stack height is volatile"
            & Output.Field_Separator
            & Name
            & Output.Field_Separator
            & Storage.Image (Height));

      end if;

      Append (
         To    => To.Stacks,
         Value => Stack_T'(
            Kind         => Unstable,
            Name         => new String'(Name),
            Height_Unit  => Unit,
            Pointer_Cell => Pointer,
            Pointer_Var  => Pointer_Var,
            Coupling     => Coupling,
            Height_Cell  => Height,
            Height_Var   => Arithmetic.Cell (Height),
            Net_Change   => Net_Change,
            Index        => Next (To.Stacks)));

      Output.Note (
           "Added unstable stack """
         & Name
         & """, pointer "
         & Arithmetic.Image (Pointer_Var)
         & " coupling by "
         & Arithmetic.Algebra.Coupling_T'Image (Coupling)
         & " to height "
         & Storage.Image (Height)
         & ", net change "
         & Storage.Bounds.Image (Net_Change, "variable")
         & ", unit "
         & Arithmetic.Image (Unit)
         & " bits.");

   end Add_Unstable_Stack;


   procedure Add_Unstable_Stack_Without_Pointer (
      Name       : in String;
      Height     : in Storage.Cell_T;
      Unit       : in Arithmetic.Positive_Value_T;
      Net_Change : in Storage.Bounds.Limit_T;
      To         : in Program_T)
   is
   begin

      Add_Unstable_Stack (
         Name       => Name,
         Pointer    => Storage.No_Cell,
         Height     => Height,
         Unit       => Unit,
         Coupling   => Arithmetic.Algebra.Coupling_T'First,
         Net_Change => Net_Change,
         To         => To);

   end Add_Unstable_Stack_Without_Pointer;


   function Number_Of_Stacks (Within : Program_T) return Natural
   is
   begin

      return Length (Within.Stacks);

   end Number_Of_Stacks;


   function Stacks (Within : Program_T) return Stacks_T
   is
   begin

      return To_Vector (Within.Stacks);

   end Stacks;


   function Stack_By (
      Index  : Stack_Index_T;
      Within : Program_T)
   return Stack_T
   is
   begin

      return Element (Within.Stacks, Index);

   exception

   when Constraint_Error =>

      raise No_Such_Stack;

   end Stack_By;


   function Stack_By (
      Name   : String;
      Within : Program_T)
   return Stack_T
   is

      Stack : Stack_T;
      -- One of the stacks in the program.

   begin

      for S in 1 .. Length (Within.Stacks) loop

         Stack := Element (Within.Stacks, S);

         if Programs.Name (Stack) = Name then

            return Stack;

         end if;

      end loop;

      raise No_Such_Stack;

   end Stack_By;


   function Stack_Name (Index : Stack_Index_T; Within : Program_T)
   return String
   is
   begin

      return Name (Element (Within.Stacks, Index));

   end Stack_Name;


   function Stack_Kind (Index : Stack_Index_T; Within : Program_T)
   return Stack_Kind_T
   is
   begin

      return Kind (Element (Within.Stacks, Index));

   end Stack_Kind;


   function Stack_Height (Index : Stack_Index_T; Within : Program_T)
   return Storage.Cell_T
   is
   begin

      return Height (Element (Within.Stacks, Index));

   end Stack_Height;


   function Stack_Height (Index : Stack_Index_T; Within : Program_T)
   return Arithmetic.Variable_T
   is
   begin

      return Height (Element (Within.Stacks, Index));

   end Stack_Height;


   function Stack_Height_Cells (Within : Program_T)
   return Storage.Cell_List_T
   is

      Heights : Storage.Cell_List_T (1 .. Length (Within.Stacks));
      -- The stack-height cells to be returned.

   begin

      for H in Heights'Range loop

         Heights(H) := Element (Within.Stacks, H).Height_Cell;

      end loop;

      return Heights;

   end Stack_Height_Cells;


   function Unstable_Stack_Height_Cells (Within : Program_T)
   return Storage.Cell_List_T
   is

      Heights : Storage.Cell_List_T (1 .. Length (Within.Stacks));
      Last    : Natural := 0;
      -- The stack-height cells to be returned, in Heights(1 .. Last).

      Stack : Stack_T;
      -- One of the stacks in the program.

   begin

      for S in 1 .. Number_Of_Stacks (Within) loop

         Stack := Element (Within.Stacks, S);

         case Stack.Kind is

         when Stable =>
            -- Not included.

            null;

         when Unstable =>
            -- Included.

            Last := Last + 1;
            Heights(Last) := Stack.Height_Cell;

         end case;

      end loop;

      return Heights(1 .. Last);

   end Unstable_Stack_Height_Cells;


   function Unstable_Stack_Heights_Zero (Within : Program_T)
   return Storage.Bounds.Cell_Interval_List_T
   is

      Cells : constant Storage.Cell_List_T :=
         Unstable_Stack_Height_Cells (Within);
      -- All the Unstable stack-height cells.

      List : Storage.Bounds.Cell_Interval_List_T (Cells'Range);
      -- The result to be.

   begin

      for C in Cells'Range loop

         List(C) := (
            Cell     => Cells(C),
            Interval => Storage.Bounds.Exactly_Zero);

      end loop;

      return List;

   end Unstable_Stack_Heights_Zero;


   function Stack_Pointer_Cells (Within : Program_T)
   return Storage.Cell_List_T
   is
      use type Storage.Cell_T;

      Pointers : Storage.Cell_List_T (1 .. Length (Within.Stacks));
      Last     : Natural := 0;
      -- The stack-pointer cells to be returned, in Pointers(1 .. Last).

      Stack : Stack_T;
      -- One of the stacks in the program.

   begin

      for S in 1 .. Number_Of_Stacks (Within) loop

         Stack := Element (Within.Stacks, S);

         if Stack.Pointer_Cell /= Storage.No_Cell then

            Last := Last + 1;
            Pointers(Last) := Stack.Pointer_Cell;

         end if;

      end loop;

      return Pointers(1 .. Last);

   end Stack_Pointer_Cells;


   function Is_Stack_Height (
      Cell   : Storage.Cell_T;
      Within : Program_T)
   return Boolean
   is
      use type Storage.Cell_T;
   begin

      for S in 1 .. Length (Within.Stacks) loop

         if Cell = Element (Within.Stacks, S).Height_Cell then

            return True;

         end if;

      end loop;

      return False;

   end Is_Stack_Height;


   procedure Set_Compiler (
      Compiler  : in Compiler_Ref;
      Generated : in Program_T)
   is
   begin

      -- TBA dereference old compiler object.

      Generated.Compiler := Compiler;

   end Set_Compiler;


   function Compiler (Item : Program_T) return Compiler_Ref
   is
   begin

      return Item.Compiler;

   end Compiler;


   procedure Set_Instruction_Role (
      Address  : in     Processor.Code_Address_T;
      Role     : in     Processor.Instruction_Role_T;
      Within   : in     Program_T;
      Conflict :    out Boolean)
   is
      use type Processor.Instruction_Role_T;

      Point : Role_Point_T;
      -- A role perhaps already set for this instruction.

   begin

      Point := Role_Maps.Search (
         Key    => Address,
         Within => Within.Instruction_Roles);
      -- Raises Nonexistent_Key if the address has not
      -- yet been assigned a role.

      -- Aha, a role has already been set here.

      Conflict := Role /= Point.Role;

   exception

   when Role_Maps.Nonexistent_Key =>
      -- This Address has not yet been assigned a role.

      if Role /= Processor.No_Role then

         Role_Maps.Insert (
            Item => (Address => Address, Role => Role, Used => False),
            Into => Within.Instruction_Roles);

      end if;

      Conflict := False;

   end Set_Instruction_Role;


   function Instruction_Role (
      Address : Processor.Code_Address_T;
      Within  : Program_T)
   return Processor.Instruction_Role_T
   is

      Point : Role_Point_T;
      -- The role set for this instruction.

   begin

      Point := Role_Maps.Search (
         Key    => Address,
         Within => Within.Instruction_Roles);
      -- Raises Nonexistent_Key if the address has not
      -- yet been assigned a role.

      -- Aha, a role has been set here.

      if not Point.Used then
         -- Mark this point "used".

         Point.Used := True;

         Role_Maps.Replace (
            Key      => Address,
            New_Item => Point,
            Within   => Within.Instruction_Roles);

      end if;

      return Point.Role;

   exception

   when Role_Maps.Nonexistent_Key =>
      -- This Address has not yet been assigned a role.

      return Processor.No_Role;

   end Instruction_Role;


   function Instruction_Role_Was_Used (
      Address : Processor.Code_Address_T;
      Within  : Program_T)
   return Boolean
   is

      Point : Role_Point_T;
      -- The role set for this instruction.

   begin

      Point := Role_Maps.Search (
         Key    => Address,
         Within => Within.Instruction_Roles);
      -- Raises Nonexistent_Key if the address has not
      -- been assigned a role.

      -- Good, a role has been set here.

      return Point.Used;

   exception

   when Role_Maps.Nonexistent_Key =>
      -- This Address has not yet been assigned a role.

      Output.Fault (
         Location => "Programs.Instruction_Role_Was_Used",
         Text     =>
              "No role set for "
            & Processor.Image (Address));

      return False;

   end Instruction_Role_Was_Used;


   --
   --    Subprogram operations:
   --


   function Index (Subprogram : Subprogram_T)
   return Subprogram_Index_T
   is
   begin

      return Subprogram.Index;

   end Index;


   function Program (Subprogram : Subprogram_T) return Program_T
   is
   begin

      return Subprogram.Program;

   end Program;


   function Symbol_Table (Subprogram : in Subprogram_T)
   return Symbols.Symbol_Table_T
   is
   begin

      return Subprogram.Program.Symbol_Table;

   end Symbol_Table;


   function Name (
      Subprogram : Subprogram_T;
      Qualified  : Boolean   := False;
      Delimiter  : Character := Symbols.Default_Delimiter)
   return String
   is
      use Ada.Strings.Unbounded;
   begin

     if      (Qualified or Opt.Qualify_Names)
     and then Symbols.Depth (Subprogram.Scope) > 0
     then

        return Symbols.Image (Subprogram.Scope, Delimiter)
           & Delimiter
           & To_String (Subprogram.Name);

     else

        return To_String (Subprogram.Name);

     end if;

   end Name;


   function Index_And_Name (Subprogram : Subprogram_T)
   return String
   is
   begin

      return '#'
         & Subprogram_Index_T'Image (Index (Subprogram))
         & " = "
         & Name (Subprogram, Qualified => True)
         & " at "
         & Processor.Image (Entry_Address (Subprogram));

   end Index_And_Name;


   function Entry_Address (Subprogram : Subprogram_T)
   return Processor.Code_Address_T
   is
   begin

      return Subprogram.Entry_Address;

   end Entry_Address;


   function Root (Subprogram : Subprogram_T) return Boolean
   is
   begin

      return Subprogram.Root;

   end Root;


   function Stub (Subprogram : Subprogram_T) return Boolean
   is
   begin

      return Subprogram.Stub;

   end Stub;


   procedure Set_Stub (
      Subprogram : in Subprogram_T;
      To         : in Boolean)
   is
   begin

      Subprogram.Stub := To;

      Output.Note (
         Locus => Locus (Subprogram),
         Text  => "Stub set to " & Boolean'Image (To));

   end Set_Stub;


   function Unused (Subprogram : Subprogram_T) return Boolean
   is
   begin

      Output.Note (
           "Unused ("
         & Index_And_Name (Subprogram)
         & ") is "
         & Boolean'Image (Subprogram.Unused));

      return Subprogram.Unused;

   end Unused;


   procedure Set_Unused (
      Subprogram : in Subprogram_T;
      To         : in Boolean)
   is
   begin

      Output.Note (
           "Set_Unused from "
         & Boolean'Image (Subprogram.Unused)
         & " to "
         & Boolean'Image (To)
         & " for "
         & Index_And_Name (Subprogram));

      Subprogram.Unused := To;

   end Set_Unused;


   function Return_Method (Subprogram : Subprogram_T)
   return Calling.Return_Method_T
   is
   begin

      if Subprogram.Unused then

         return (Way => Calling.No_Return);

      else

         return Subprogram.Return_Method;

      end if;

   end Return_Method;


   function Returns (Subprogram : Subprogram_T) return Boolean
   is
      use Calling;
   begin

      return Return_Method (Subprogram).Way /= No_Return;

   end Returns;


   procedure Set_Return_Method (
      Subprogram : in Subprogram_T;
      To         : in Calling.Return_Method_T)
   is
   begin

      Subprogram.Return_Method := To;

   end Set_Return_Method;


   function Integrate_Calls (Subprogram : Subprogram_T) return Boolean
   is
   begin

      return Subprogram.Integrate_Calls and not Subprogram.Unused;

   end Integrate_Calls;


   procedure Set_Call_Integration (
      Subprogram : in Subprogram_T;
      To         : in Boolean)
   is
   begin

      Subprogram.Integrate_Calls := To;

   end Set_Call_Integration;


   procedure Set_Arithmetic_Analysis (
      Subprogram : in Subprogram_T;
      Choice     : in Arithmetic.Opt.Choice_T)
   is
      use type Arithmetic.Opt.Choice_T;
   begin

      if Subprogram.Arith_Choice /= Arithmetic.Opt.Undefined then

         Output.Warning ("Conflicting ""arithmetic"" assertions.");

      end if;

      Subprogram.Arith_Choice := Choice;

   end Set_Arithmetic_Analysis;


   function Arithmetic_Analysis (Subprogram : Subprogram_T)
   return Arithmetic.Opt.Choice_T
   is
      use type Arithmetic.Opt.Choice_T;
   begin

      if Subprogram.Arith_Choice /= Arithmetic.Opt.Undefined then
         -- Asserted for this subprogram.

         return Subprogram.Arith_Choice;

      else
         -- Not asserted for this subprogram; use general option.

         return Arithmetic.Opt.Analysis;

      end if;

   end Arithmetic_Analysis;


   function Hide_In_Call_Graph_Drawing (Subprogram : Subprogram_T)
   return Boolean
   is
   begin

      return Subprogram.Hide_In_Drawing or Subprogram.Unused;

   end Hide_In_Call_Graph_Drawing;


   procedure Set_Hiding_In_Call_Graph_Drawing (
      Subprogram : in Subprogram_T;
      To         : in Boolean)
   is
   begin

      Subprogram.Hide_In_Drawing := To;

   end Set_Hiding_In_Call_Graph_Drawing;


   function Flow_Graph (Subprogram : Subprogram_T)
   return Flow.Graph_T
   is
   begin

      return Subprogram.Flow_Graph;

   end Flow_Graph;


   function Locus (
      Step   : Flow.Step_T;
      Within : Subprogram_T)
   return Output.Locus_T
   is
   begin

      return Flow.Show.Locus (
         Step   => Step,
         Source => Symbol_Table (Within));

   end Locus;


   procedure Set_Loops (
      Within : in Subprogram_T;
      To     : in Loops.Loops_T)
   is
   begin

      Within.Reducible := True;
      Within.Loop_Info := new Loops.Loops_T'(To);

   end Set_Loops;


   procedure Set_Irreducible (
      Subprogram : in Subprogram_T)
   is
   begin

      Subprogram.Reducible := False;
      Subprogram.Loop_Info := null;

   end Set_Irreducible;


   function Reducible (Subprogram : Subprogram_T) return Boolean
   is
   begin

      return Subprogram.Reducible;

   end Reducible;


   function Number_Of_Loops (Subprogram : Subprogram_T)
   return Loops.Loop_Count_T
   is
      use type Loops.Loops_Ref;
   begin

      if Subprogram.Loop_Info /= null then

         return Subprogram.Loop_Info'Length;

      else

         return 0;

      end if;

   end Number_Of_Loops;


   function Loops_Of (Subprogram : Subprogram_T) return Loops.Loops_T
   is
      use type Loops.Loops_Ref;

      Empty_Loop_Info : Loops.Loops_T(1..0);

   begin

      if Subprogram.Loop_Info /= null then

         return Subprogram.Loop_Info.all;

      else

         return Empty_Loop_Info;

      end if;

   end Loops_Of;


   function Containing_Loops (
      Luup   : Loops.Loop_T;
      Within : Subprogram_T)
   return Loops.Loop_List_T
   is

      Containers : constant Loops.Loop_List_T :=
         Loops.Containing_Loops (
            Loops => Loops.All_Loops (Loops_Of (Within)),
            Node  => Loops.Head_Node (Luup));
      -- The containing loops and the Luup itself.

   begin

      return Containers(Containers'First .. Containers'Last - 1);

   end Containing_Loops;


   function Locus (
      Luup   : Loops.Loop_T;
      Within : Subprogram_T)
   return Output.Locus_T
   is
   begin

      return Loops.Show.Locus (
         Luup   => Luup,
         Within => Flow_Graph (Within),
         Source => Symbol_Table (Within));

   end Locus;


   function Number_Of_Calls_From (Caller : Subprogram_T) return Natural
   is
   begin

      return Call_Vectors.Length (Caller.Calls);

   end Number_Of_Calls_From;


   function Calls_From (Caller : Subprogram_T) return Call_List_T
   is
   begin

      return Call_Vectors.To_Vector (Caller.Calls);

   end Calls_From;


   function Call (From : Subprogram_T; Index : Call_Index_T) return Call_T
   is
   begin

      return Call_Vectors.Element (From.Calls, Index);

   end Call;


   function Calls_To (Callee : Subprogram_T)
   return Call_List_T
   is
      use Call_Vectors;

      Program : Program_T renames Callee.Program;

      Result : Call_List_T (1 .. Length (Program.Calls));
      Last   : Natural := 0;
      -- The result will be Result(1 .. Last).

      Cand : Call_T;
      -- Candidate for the result; one of the calls.

   begin

      for C in 1 .. Length (Program.Calls) loop

         Cand := Element (Program.Calls, C);

         if  Cand.Caller /= No_Subprogram
         and Cand.Callee  = Callee
         then
            -- We found one of them.

            Last := Last + 1;
            Result(Last) := Cand;

         end if;

      end loop;

      return Result(1 .. Last);

   end Calls_To;


   function Calls (
      From : Subprogram_T;
      Into : Subprogram_Set_T)
   return Call_List_T
   is

      All_Calls: Call_List_T := Calls_From (Caller => From);
      -- All the calls from the given subprogram.

      Calls_Into : Call_List_T (1 .. All_Calls'Length);
      -- The calls that go into the given subprogram set are
      -- placed in 1 .. Last.

      Last : Natural := 0;
      -- The number of suitable calls found.

   begin

      for C in All_Calls'Range loop

         if Is_Member (Callee (All_Calls(C)), Into) then

            Last := Last + 1;
            Calls_Into(Last) := All_Calls(C);

         end if;

      end loop;

      return Calls_Into (1 .. Last);

   end Calls;


   function Some_Calls (
      From : Subprogram_T;
      Into : Subprogram_Set_T)
   return Boolean
   is

      The_Calls : constant Call_List_T :=
         Calls (From => From, Into => Into);
      -- The calls from the given subprogram to some subprogram in
      -- the given set.

   begin

      return The_Calls'Length > 0;

   end Some_Calls;


   function Calls_Between (
      Program     : Program_T;
      Subprograms : Subprogram_Set_T)
   return Call_List_T
   is

      All_Calls : constant Call_List_T := Calls(Program);
      Call_List : Call_List_T (1 .. All_Calls'Length);
      Last      : Natural := 0;

   begin

      for C in All_Calls'Range loop

         if Caller(All_Calls(C)) /= null then
            -- A "real" call.

            if  Is_Member(Caller(All_Calls(C)), Subprograms)
            and Is_Member(Callee(All_Calls(C)), Subprograms)
            then

               Last := Last + 1;
               Call_List(Last) := All_Calls(C);

            end if;

         end if;

      end loop;

      return Call_List(1..Last);

   end Calls_Between;


   function Processor_Info (Sub : Subprogram_T)
   return Processor.Program.Sub_Info_T
   is
   begin

      return Sub.Processor_Info;

   end Processor_Info;


   procedure Set_Processor_Info (
      Sub : in Subprogram_T;
      To  : in Processor.Program.Sub_Info_T)
   is
   begin

      Sub.Processor_Info := To;

   end Set_Processor_Info;


   function Processor_Info (Sub : Subprogram_T)
   return Processor.Program.Info_T
   is
   begin

      return Processor_Info (Program (Sub));

   end Processor_Info;


   function Initial_Stack_Height (
      Stack    : Stack_T;
      Entering : Subprogram_T)
   return Arithmetic.Value_T
   is
   begin

      case Stack.Kind is

      when Stable =>
         -- We refer to Entry_Bounds.

         declare

            Interval : constant Storage.Bounds.Interval_T :=
               Storage.Bounds.Interval (
                  Cell => Stack.Height_Cell,
                  From => Processor.Properties.Entry_Bounds (
                     Subprogram => Entering,
                     Sub_Info   => Processor_Info (Entering)));
            -- Entry bounds for the stack-height cell.

         begin

            if not Storage.Bounds.Singular (Interval) then

               Output.Fault (
                  Location => "Programs.Initial_Stack_Height",
                  Text     =>
                       "The initial stack height for "
                     & Stack.Name.all
                     & " is variable"
                     & Output.Field_Separator
                     & Storage.Bounds.Image (
                          Item => Interval,
                          Name => Storage.Image (Stack.Height_Cell)));

               raise Program_Error;

            end if;

            return Storage.Bounds.Single_Value (Interval);

         end;

      when Unstable =>
         -- It is axiomatically zero.

         return 0;

      end case;

   end Initial_Stack_Height;


   function Locus (
      Subprogram : Subprogram_T;
      Qualified  : Boolean := False)
   return Output.Locus_T
   is
      use type Flow.Step_Count_T;
      use type Output.Locus_T;

      Graph : constant Flow.Graph_T := Flow_Graph (Subprogram);
      -- The flow-graph of the subprogram, if defined.

      New_Max_Step : constant Flow.Step_Count_T := Flow.Max_Step (Graph);
      -- The number of steps in the Graph, now.

   begin

      if New_Max_Step /= Subprogram.Locus_Max_Step then
         -- The flow-graph has been extended since the locus was
         -- last computed. Recompute the locus and remember it.

         if New_Max_Step < Subprogram.Locus_Max_Step then
            -- Eh?

            Output.Fault (
               Location => "Programs.Locus (Subprogram_T)",
               Text     => "Max_Step decreased");

         end if;

         Subprogram.Locus := Flow.Show.Locus (
            Graph  => Graph,
            Source => Symbol_Table (Subprogram));

         Subprogram.Locus_Max_Step := New_Max_Step;

      end if;

      return Output.Locus (
                Call_Path => Name (Subprogram, Qualified => Qualified))
           & Subprogram.Locus;

   end Locus;


   function Entry_Locus (
      Subprogram : Subprogram_T;
      Qualified  : Boolean := False)
   return Output.Locus_T
   is
   begin

      return
         Output.Locus (
            Call_Path  => Name (Subprogram, Qualified => Qualified),
            Statements =>
               Symbols.Show.Statements (
                  Address => Entry_Address (Subprogram),
                  Source  => Program (Subprogram).Symbol_Table));

   end Entry_Locus;


   --
   --    Call operations
   --


   function Program (Call : Call_T) return Program_T
   is
   begin

      return Program (Caller (Call));

   end Program;


   function Image (Call : Call_T) return String
   is
   begin

      if Call = No_Call then

         return "";

      elsif Caller (Call) /= null then

         return Name (Caller (Call)) & Call_Arrow (Call);

      else

         return "Root " & Name (Callee (Call));

      end if;

   end Image;


   function Caller (Call : Call_T) return Subprogram_T
   is
   begin

      return Call.Caller;

   end Caller;


   function Callee (Call : Call_T) return Subprogram_T
   is
   begin

      return Call.Callee;

   end Callee;


   function Unused (Call : Call_T) return Boolean
   is
   begin

      return Unused (Callee (Call));

   end Unused;


   function Index (Call : Call_T) return Call_Index_T
   is
   begin

      return Call.Index;

   end Index;


   function Step (Call : Call_T) return Flow.Step_T
   is
   begin

      return Call.Flow_Step;

   end Step;


   function Steps (Calls : Call_List_T) return Flow.Step_List_T
   is
      Result : Flow.Step_List_T (Calls'Range);
   begin

      for C in Calls'Range loop

         Result(C) := Calls(C).Flow_Step;

      end loop;

      return Result;

   end Steps;


   function Node (Call : Call_T) return Flow.Node_T
   is
   begin

      return Flow.Node_Containing (
         Step  => Step (Call),
         Graph => Flow_Graph (Caller (Call)));

   end Node;


   function Nodes (Calls : Call_List_T) return Flow.Node_List_T
   is
      Result : Flow.Node_List_T (Calls'Range);
   begin

      for C in Calls'Range loop

         Result(C) := Node (Calls(C));

      end loop;

      return Result;

   end Nodes;


   function Take_Off_Node (Call : Call_T) return Flow.Node_T
   is
   begin

      return Take_Off_Node (
         Call   => Node (Call),
         Within => Flow_Graph (Caller (Call)));

   end Take_Off_Node;


   function Take_Off_Node (
      Call   : Flow.Node_T;
      Within : Flow.Graph_T)
   return Flow.Node_T
   is

      Preds : constant Flow.Node_List_T := Flow.Predecessors (
         Node   => Call,
         Within => Within);
      -- All the predecessors of the call-node.

   begin

      if Preds'Length /= 1 then

         Output.Fault (
            Location => "Programs.Take_Off_Node",
            Text     =>
                 "Call-node has"
               & Natural'Image (Preds'Length)
               & " predecessor nodes.");

      end if;

      return Preds(Preds'First);

   end Take_Off_Node;


   function Prime_Address (Call : Call_T) return Processor.Code_Address_T
   is
   begin

      return Flow.Prime_Address (Step (Call));

   end Prime_Address;


   function Protocol (Call : Call_T) return Calling.Protocol_Ref
   is
   begin

      return Call.Protocol;

   end Protocol;


   procedure Add_Root (
      Root : in     Subprogram_T;
      To   : in     Program_T;
      Call :    out Call_T)
   is
   begin

      if Unused (Root) then
         -- Reject this.

         Output.Error (
            Locus => Locus (Root),
            Text  => "Root subprogram cannot be ""unused"".");

      else
         -- Mark the subprogram as a root:

         Root.Root := True;

	 -- Make the virtual call:

	 Call := new Call_Object_T;

	 Call.Caller := null;
	 Call.Callee := Root;

	 -- TBM: No Step for root Call.

	 -- Every root subprogram is considered as "called":

	 Root.Called := True;

	 -- But although this is (or should be) the first call to
	 -- the Root subprogram, a root is never considered a shoot.

	 -- Enter the call in the set of all calls and in the
	 -- set of root calls:

	 Call_Vectors.Append (To => To.Calls, Value => Call);

	 Call_Vectors.Append (To => To.Roots, Value => Call);

         Processor.Properties.Define_As_Root (
            Subprogram => Root,
            Info       => Root.Processor_Info);

      end if;

   end Add_Root;


   function Root_Subprograms (Prog : Program_T)
   return Subprogram_List_T
   is

      Calls : constant Call_List_T := Root_Calls (Prog);
      -- The root calls.

      Roots : Subprogram_List_T (Calls'Range);
      -- The root subprograms.

   begin

      for R in Roots'Range loop

         Roots(R) := Callee (Calls(R));

      end loop;

      return Roots;

   end Root_Subprograms;


   function Root_Calls (Prog : Program_T)
   return Call_List_T
   is
   begin

      return Call_Vectors.To_Vector (Prog.Roots);

   end Root_Calls;


   function Root_Call (
      Address : Processor.Code_Address_T;
      Within  : Program_T)
   return Call_T
   is
      use type Processor.Code_Address_T;

      Roots : constant Call_List_T := Root_Calls (Within);

   begin

      for R in Roots'Range loop

         if Entry_Address (Callee (Roots(R))) = Address then

            return Roots(R);

         end if;

      end loop;

      -- No root call found.

      raise Subprogram_Not_Found;

   end Root_Call;


   function Locus (Call : Call_T) return Output.Locus_T
   is

      Loc : Output.Locus_T;
      -- The result.

   begin

      if Caller (Call) /= null then
         -- Not a root call.

         Loc :=
            Output.Locus (
               Call_Path => Image (Call),
               Statements =>
                  Flow.Show.Statements (
                     Step   => Step (Call),
                     Source => Program (Call).Symbol_Table));

         Output.Add_Surrounding_Lines (To => Loc);

     else
         -- A root call.

         Loc := Locus (Callee (Call));

      end if;

      return Loc;

   end Locus;


   function Is_Looped (Call : Call_T) return Boolean
   --
   -- Whether the Call is in some loop.
   --
   is

      Luups : constant Loops.Loops_T := Loops_Of (Caller (Call));
      -- All the loops in the relevant subprogram.

      Call_Node : constant Flow.Node_T := Node (Call);
      -- The node that contains the call.

   begin

      for L in Luups'Range loop

         if Loops.Contains (Luups(L), Call_Node) then
            -- The Call is contained in this loop.

            return True;

         end if;

      end loop;

      return False;

   end Is_Looped;


   function Non_Looped_Calls (Subprogram : Subprogram_T)
   return Call_List_T
   is

      Calls : Call_List_T := Calls_From (Caller => Subprogram);
      -- All the calls.
      -- We will winnow this list to remove the looped calls.

      Last : Natural := Calls'First - 1;
      -- The non-looped calls are Calls(Calls'First .. Last).

   begin

      for C in Calls'Range loop

         if not Is_Looped (Calls(C)) then
            -- This one we keep.

            Last := Last + 1;

            if Last < C then

               Calls(Last) := Calls(C);

            end if;

         end if;

      end loop;

      return Calls(Calls'First .. Last);

   end Non_Looped_Calls;


   function Containing_Loops (Call : Call_T)
   return Loops.Loop_List_T
   is
   begin

      return Loops.Containing_Loops (
         Loops => Loops.All_Loops (Loops_Of (Caller (Call))),
         Node  => Node (Call));

   end Containing_Loops;


   function Calls_Within (
      Luup : Loops.Loop_T;
      From : Subprogram_T)
   return Call_List_T
   is

      List : Call_List_T := Calls_From (Caller => From);
      -- All the calls from this subprogram.

      Last : Natural := List'First - 1;
      -- We will return List(List'First .. Last).

   begin

      for L in List'Range loop

         if Loops.Contains (Luup, Node (List(L))) then
            -- This call is in the Luup.

            Last := Last + 1;

            if Last < L then

               List(Last) := List(L);

            end if;

         end if;

      end loop;

      return List(List'First .. Last);

   end Calls_Within;


   --
   --    Subprogram Set operations
   --


   procedure Erase (Set : in out Subprogram_Set_T)
   is
   begin

      if Set /= null then

         Subprogram_Bags.Destroy (Subprog_Set_T (Set.all));

      end if;

   end Erase;


   function Is_Empty (Set : Subprogram_Set_T) return Boolean
   is
   begin

      return  Set = null
      or else Subprogram_Bags.Empty (Subprog_Set_T (Set.all));

   end Is_Empty;


   function Is_Member (
      Subprogram : Subprogram_T;
      Of_Set     : Subprogram_Set_T)
   return Boolean
   is
   begin

      return Of_Set /= null
      and then
         Subprogram_Bags.Member (
            Key    => Entry_Address (Subprogram),
            Of_Bag => Subprog_Set_T (Of_Set.all));

   end Is_Member;


   function Cardinality (Set : Subprogram_Set_T) return Natural
   --
   -- The number of subprograms in the set.
   is
   begin

      if Set = null then

         return 0;

      else

         return Natural(Subprogram_Bags.Card(Subprog_Set_T(Set.all)));

      end if;

   end Cardinality;


   procedure Add (
     To     : in out Subprogram_Set_T;
     Adding : in     Subprogram_T)
   is
   begin

      if To = null then
         To := new Subprogram_Set_Object_T;
      end if;

      Subprogram_Bags.Insert (
         Item => Adding,
         Into => Subprog_Set_T (To.all));

   exception

   when Subprogram_Bags.Duplicate_Key =>
      -- It is already in the set, no huhu, OK.

      null;

   end Add;


   procedure Add (
     To     : in out Subprogram_Set_T;
     Adding : in     Subprogram_Set_T)
   is
   begin

      if Adding = null then

         return;

      end if;

      declare

         Add_List : constant Subprogram_Bags.List :=
            Subprogram_Bags.To_List (Subprog_Set_T (Adding.all));

      begin

         for A in Add_List'range loop

            Add (To => To, Adding => Add_List(A));

         end loop;

      end;

   end Add;


   procedure Remove (
     From     : in out Subprogram_Set_T;
     Removing : in     Subprogram_T)
   is
   begin

      if From /= null then

         Subprogram_Bags.Remove (
            Key  => Entry_Address (Removing),
            From => Subprog_Set_T (From.all));

      end if;

   exception

   when Subprogram_Bags.Nonexistent_Key =>
      -- It was not in the set, no huhu, OK.

      null;

   end Remove;


   procedure Remove (
      From     : in out Subprogram_Set_T;
      Removing : in     Subprogram_Set_T)
   is
   begin

      if Removing = null then

         return;

      end if;

      declare

         Remove_List : constant Subprogram_Bags.List :=
            Subprogram_Bags.To_List (Subprog_Set_T (Removing.all));

      begin

         if From /= null then

            for R in Remove_List'range loop

               Remove (From => From, Removing => Remove_List(R));

            end loop;

         end if;

      end;

   end Remove;


   function To_List (Item : Subprogram_Set_T) return Subprogram_List_T
   is
   begin

      if Item = null then

         return Empty_Subprogram_List;

      else

         return Subprogram_List_T (
            Subprogram_Bags.To_List (Subprog_Set_T (Item.all)));

      end if;

   end To_List;


   function New_Named_Subprogram (
      Program : Program_T;
      Address : Processor.Code_Address_T;
      Info    : Processor.Program.Sub_Info_T)
   return Subprogram_T
   --
   -- A new subprogram with the given entry Address in a given
   -- Program and with the given target-specific Info.
   --
   -- Note that this function does _not_ search the subprograms already
   -- created in this program. This function should be called only when
   -- no subprogram in the program already has this entry Address.
   -- Otherwise there will be multiple subprogram objects with the
   -- same entry address, making all calls to this address ambiguous.
   -- In fact, the insertion of such subprograms into the subprogram set
   -- will fail because it does not allow duplicate keys.
   --
   -- The function tries to find the entry address in the symbol table
   -- and so to set the correct Scope and Name for the new subprogram.
   --
   -- If no subprogram connection with this Address is found, we create
   -- a subprogram from scratch, with a null Scope and a Name equal to
   -- the Processor.Image of Address (usually a hexadecimal number).
   --
   -- If several subprogram connections for this Address are found, the
   -- first one is used and warning message are issued for the rest.
   --
   is
      use Symbols;

      Connections : Connection_Set_T :=
         Connections_For_Address (Address, Program.Symbol_Table);
      -- All the connections for this Address.

      New_Sub : Subprogram_T;
      -- The new subprogram.

      Matches : Natural := 0;
      -- The number of matching connections found.

      First_Match : Connection_T;
      -- The first matching connection.


      procedure Report (
         Match : in Connection_T;
         Usage : in String)
      --
      -- Reports a matching connection, whether used or ignored.
      --
      is
      begin

         Output.Note (
              Usage
            & " symbol for subprogram at address "
            & Processor.Image (Address)
            & Output.Field_Separator
            & Image (Scope_Of (Match))
            & Symbols.Default_Delimiter
            & Name_Of (Match));

      end Report;


   begin

      -- Look for a Subprogram connection at Address:

      for C in Connections'Range loop

         if Kind_Of (Connections(C)) = Subprogram then

            Matches := Matches + 1;

            if Matches = 1 then
               -- First and hopefully only connection for Address.

               First_Match := Connections(C);

               New_Sub :=
                  New_Subprogram (
                     Scope   => Scope_Of   (First_Match),
                     Name    => Name_Of    (First_Match),
                     Address => Address_Of (First_Match),
                     Info    => Info,
                     Within  => Program);

            else
               -- One of the multiple connections for Address.

               if Matches = 2 then
                  -- Report the first (used) match once:

                  Report (
                     Match => First_Match,
                     Usage => "Using");

               end if;

               -- Report every other (ignored) match:

               Report (
                  Match => Connections(C),
                  Usage => "Other");

            end if;

         end if;

      end loop;

      -- Did we find one?

      if Matches = 0 then
         -- There are no Subprogram connections for Address.

         Output.Note (
              "No subprogram-symbol for entry address "
            & Processor.Image (Address));

         -- Twist a rough subprogram from steel wire:

         New_Sub :=
            New_Subprogram (
               Scope   => Scope_Of (""),
               Name    => Processor.Image (Address),
               Address => Address,
               Info    => Info,
               Within  => Program);

      end if;

      return New_Sub;

   end New_Named_Subprogram;


   procedure Identify_Callee (
      Caller : in     Subprogram_T;
      Target : in     Processor.Code_Address_T;
      Info   : in     Processor.Program.Sub_Info_T;
      Callee :    out Subprogram_T)
   is
   begin

      if Subprogram_Bags.Member (
            Key    => Target,
            Of_Bag => Caller.Program.Subprograms)
      then
         -- The callee subprogram has been accessed before.

         Callee := Subprogram_Bags.Search (
            Key    => Target,
            Within => Caller.Program.Subprograms);

         Output.Note (
              "Call to existing subprogram "
            & Index_And_Name (Callee));

         -- Update and verify the processor-specific information
         -- for the existing Callee, using the processor-specific
         -- information about this call:

         Processor.Properties.Inform_By_Call (
            Caller      => Caller,
            Callee      => Callee,
            Call_Info   => Info,
            Callee_Info => Callee.Processor_Info);

      else
         -- The callee subprogram is a new one.

         Callee := New_Named_Subprogram (
            Program => Caller.Program,
            Address => Target,
            Info    => Info);

         Output.Note (
              "Call to new subprogram "
            & Index_And_Name (Callee));

      end if;

   end Identify_Callee;


   procedure Add_Call (
      Caller   : in     Subprogram_T;
      Callee   : in     Subprogram_T;
      Protocol : in     Calling.Protocol_Ref;
      Step     : in     Flow.Step_T;
      Giving   :    out Call_T)
   is
   begin

      -- Create the call:

      Giving := new Call_Object_T'(
         Caller     => Caller,
         Callee     => Callee,
         Index      => Call_Vectors.Next (Caller.Calls),
         Flow_Step  => Step,
         Protocol   => Protocol);

      -- Is this a new shoot of the call tree?

      if not Callee.Called then
         -- This is the first call: the callee is a "shoot".

         Add (To => Callee.Program.Shoots, Adding => Callee);

         Callee.Called := True;

      end if;

      -- Record the new call:

      Call_Vectors.Append (
         To    => Caller.Calls,
         Value => Giving);

      Call_Vectors.Append (
         To    => Caller.Program.Calls,
         Value => Giving);

   end Add_Call;


   -- Private operations for use by child packages:


   function Call_Arrow (Call : Call_T) return String
   is

      Call_Statements : Output.Statement_Range_T;
      -- The call-step statement(s).

   begin

      Call_Statements :=
         Flow.Show.Statements (
            Step   => Step (Call),
            Source => Program (Call).Symbol_Table);

      Output.Add_Surrounding_Lines (To => Call_Statements);

      return
           Output.Call_Locus_Mark
         & Output.Image (Call_Statements)
         & Output.Call_Mark
         & Name (Callee (Call));

   end Call_Arrow;


end Programs;
