-- ILP (Body)
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
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: ilp.adb,v $
-- Revision 1.13  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.12  2013/12/23 09:47:31  niklas
-- Updated for changes to Exec_Cmd.
--
-- Revision 1.11  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.10  2008/09/24 08:38:52  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.9  2007/12/17 13:54:38  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.8  2007/04/24 11:02:29  niklas
-- Changed Trace_IO output from Notes to Traces.
--
-- Revision 1.7  2005/06/28 08:33:11  niklas
-- Using Output.Fault for rest of lp_solve output.
--
-- Revision 1.6  2005/06/28 06:22:22  niklas
-- Updated for changes in Exec_Cmd: better handling of exit codes.
-- Emits Error rather than Fault for "infeasible" exit code and in
-- this case does not echo the lp_solve output to standard error.
--
-- Revision 1.5  2005/04/17 07:58:41  niklas
-- Changed lp_solve problems to report Fault instead of Error.
-- The rest of the lp_solve output is still displayed as Error
-- lines for brevity.
--
-- Revision 1.4  2004/05/01 10:29:35  niklas
-- First Tidorum version.
-- Added support for target-specific ILP variables.
-- Added procedure to Add two Expressions.
-- Added constant Zero_Expression.
-- Added support for the Keep_Files option.
--
-- Revision 1.3  2003/03/11 08:26:42  holsti
-- Major update to make the types Var_Term_T, Expression_T and
-- Solver_T private, to provide operations on these types that
-- are easier to use and better encapsulated, and to avoid
-- using the heap.
--
-- Revision 1.2  2002/12/30 13:12:45  holsti
-- Context clause for ILP.Platform added.
--
-- Revision 1.1  2001/11/19 11:10:18  saarinen
-- First versions.
--

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants;

with ILP.Opt;
with ILP.Platform;
with Options.Strings;
with Output;


package body ILP is


   -- This implementation uses the ILP program lp_solve, by
   -- Michel Berkelaar, running it as a child process with
   -- text I/O through pipes.


   function Trim_Left (Item : String) return String
   --
   -- The string with leading blanks removed.
   --
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin

      return Trim (Item, Left);

   end Trim_Left;


   function Image (Item : Number_T) return String
   --
   -- Decimal digits, trimmed.
   --
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin

      return Trim (Number_T'Image (Item), Left);

   end Image;


   --
   --    Variables
   --

   function "*" (Left : Number_T; Right : Var_T)
   return Var_Term_T
   is
   begin

      return (Coefficient => Left, Variable => Right);

   end "*";


   function "*" (Left : Number_T; Right : Var_Term_T)
   return Var_Term_T
   is
   begin

      return (
         Coefficient => Left * Right.Coefficient,
         Variable    =>        Right.Variable);

   end "*";

   --
   --    Affine expressions
   --

   procedure Erase (Item : in out Expression_T)
   is
   begin

      Item.Const_Term := 0;
      Item.Num_Vars   := 0;

   end Erase;


   procedure Add (
      Const : in     Number_T;
      To    : in out Expression_T)
   is
   begin

      To.Const_Term := To.Const_Term + Const;

   end Add;


   procedure Add (
      Term : in     Var_Term_T;
      To   : in out Expression_T)
   is
   begin

      if Term.Coefficient = 0 then
         -- We ignore this term since it has a zero coefficient.

         null;

      elsif To.Num_Vars < To.Max_Vars then
         -- Still room for this new term.

         To.Num_Vars := To.Num_Vars + 1;

         To.Var(To.Num_Vars) := Term;

      else
         -- Maximum number of terms in use, cannot add more.

         Output.Fault (
            Location => "ILP.Add",
            Text =>
                 "Expression has "
               & Natural'Image (To.Num_Vars)
               & " terms, cannot add a term.");

         raise Too_Many_Terms;

      end if;

   end Add;


   procedure Add (
      Expr : in     Expression_T;
      To   : in out Expression_T)
   is
   begin

      Add (Const => Expr.Const_Term, To => To);

      for V in 1 .. Expr.Num_Vars loop

         Add (Term => Expr.Var(V), To => To);

      end loop;

   end Add;


   function "*" (Left : Number_T; Right : Expression_T)
   return Expression_T
   is

      Result : Expression_T (Max_Vars => Right.Max_Vars);

   begin

      Result.Const_Term := Left * Right.Const_Term;

      Result.Num_Vars := Right.Num_Vars;

      for V in 1 .. Result.Num_Vars loop

         Result.Var(V) := Left * Right.Var(V);

      end loop;

      return Result;

   end "*";


   function Sum_Of (
      Nodes : Flow.Node_List_T;
      Const : Number_T)
   return Expression_T
   is

      Sum : Expression_T (Nodes'Length);

   begin

      for N in Nodes'Range loop

         Add (
            Term => 1 * Var_T'(Node, Flow.Index (Nodes(N))),
            To   => Sum);

      end loop;

      Add (Const => Const, To => Sum);

      return Sum;

   end Sum_Of;


   function Sum_Of (
      Edges : Flow.Edge_List_T;
      Const : Number_T)
   return Expression_T
   is

      Sum : Expression_T (Edges'Length);

   begin

      for E in Edges'Range loop

         Add (
            Term => 1 * Var_T'(Edge, Flow.Index (Edges(E))),
            To   => Sum);

      end loop;

      Add (Const => Const, To => Sum);

      return Sum;

   end Sum_Of;


   function Sum_Of (
      Edges : Flow.Step_Edge_List_T;
      Const : Number_T)
   return Expression_T
   is

      Sum : Expression_T (Edges'Length);

   begin

      for E in Edges'Range loop

         Add (
            Term => 1 * Var_T'(Step_Edge, Flow.Index (Edges(E))),
            To   => Sum);

      end loop;

      Add (Const => Const, To => Sum);

      return Sum;

   end Sum_Of;



   --
   --    Producing ILP problems for the ILP solver
   --
   -- The interface to the ILP solver is a textual definition of
   -- the variables, the constraints, and the function to be
   -- maximized. The ILP solver runs as a child process, with input
   -- and output via pipes. The interface and textual syntax are partly
   -- encapsulated and hidden by the following types and operations.


   Node_Mark      : constant Character := 'N';
   Edge_Mark      : constant Character := 'E';
   Step_Mark      : constant Character := 'S';
   Step_Edge_Mark : constant Character := 'D';
   Target_Mark    : constant Character := 'T';
   --
   -- The first character in the ILP symbol (identifier) for a variable.
   -- This mark indicates the kind of entity the variable represents.


   Var_Mark : constant array (Var_Kind_T) of Character := (
      Node      => Node_Mark,
      Edge      => Edge_Mark,
      Step      => Step_Mark,
      Step_Edge => Step_Edge_Mark,
      Target    => Target_Mark);
   --
   -- The variable marks indexed by variable kind.


   subtype Lp_Id_T is Positive;

   Next_LP_Id : Lp_Id_T := 1;

   -- An ID for every LP_Solve run so that the input/output debug
   -- files can have unique names.


   function Blanks (Length : Natural) return String
   --
   -- The given number of blanks in a string.
   --
   is
   begin

      return (1 .. Length => ' ');

   end Blanks;


   procedure Put (Solver : in out Solver_T; Item : in String)
   --
   -- Add the Item to the textual definition of the ILP problem.
   --
   is
   begin

      if Opt.Trace_IO then

         Output.Trace (
              "ILP def"
            & Output.Field_Separator
            & Item);

      end if;

      Write (
         Exec => Solver,
         Str  => Item);

   end Put;


   procedure Put_Line (Solver : in out Solver_T; Item : in String)
   --
   -- Add the Item and a new-line to the textual definition of the
   -- ILP problem.
   --
   is
   begin

      if Opt.Trace_IO then

         Output.Trace (
              "ILP def"
            & Output.Field_Separator
            & Item
            & Output.Field_Separator
            & "newline");

      end if;

      Write_Line (
         Exec => Solver,
         Str  => Item);

   end Put_Line;


   procedure Comment (
      Solver : in out Solver_T;
      Text   : in String;
      Indent : in Natural := 0)
   is
   begin

      Put_Line (Solver,
         Blanks (Indent) & "/* " & Text & " */");

   end Comment;


   procedure Get_Line (
      Solver : in out Solver_T;
      Item   :    out Ada.Strings.Unbounded.Unbounded_String)
   --
   -- Read a line of output from the ILP solver.
   --
   is
   begin

      Read_Line (Solver, Item);

      if Opt.Trace_IO then

         Output.Trace (
              "ILP result"
            & Output.Field_Separator
            & Ada.Strings.Unbounded.To_String (Item));

      end if;

   end Get_Line;



   package Exit_Codes
   --
   -- Exit codes from lp_solve.
   -- These are from lp_solve, lpkit.h.
   --
   is

      Optimal    : constant Exec_Cmd.Exit_Code_T := 0;
      MILP_Fail  : constant Exec_Cmd.Exit_Code_T := 1;
      Infeasible : constant Exec_Cmd.Exit_Code_T := 2;
      Unbounded  : constant Exec_Cmd.Exit_Code_T := 3;
      Failure    : constant Exec_Cmd.Exit_Code_T := 4;
      Running    : constant Exec_Cmd.Exit_Code_T := 5;

   end Exit_Codes;


   function Number (Item : Var_T) return Number_T
   --
   -- The "index" of the variable.
   --
   is
   begin

      case Item.Kind is

      when Node      => return Number_T (Item.Node_Index);
      when Step      => return Number_T (Item.Step_Index);
      when Edge      => return Number_T (Item.Edge_Index);
      when Step_Edge => return Number_T (Item.Step_Edge_Index);
      when Target    => return Number_T (Item.Index);

      end case;

   end Number;


   function Image (Item : Var_T) return String
   is
   begin

      if Item.Kind = Target then

         return Var_Mark (Item.Kind) & Item.Prefix & Image (Number (Item));

      else

         return Var_Mark(Item.Kind) & Image (Number (Item));

      end if;

   end Image;


   function Image (Item : Var_Term_T) return String
   --
   -- The LP_Solve expression for the given variable term,
   -- including the coefficient and the variable identifier.
   -- Some special cases are optimized.
   --
   is
   begin

      if Item.Coefficient = 0 then

         return "0";

      elsif Item.Coefficient = 1 then

         return Image (Item.Variable);

      else

         return
              Image (Item.Coefficient)
            & ' '
            & Image (Item.Variable);

      end if;

   end Image;


   procedure Put (
      Solver : in out Solver_T;
      Item   : in     Expression_T;
      Indent : in     Natural := 0)
   --
   -- Emits the LP_Solve expression for the given affine expression.
   --
   is

      Add_Op : String(1..2) := "  ";
      -- Operator symbol to combine the current term with
      -- the preceding terms (if any). Blank for the first
      -- term, "+ " for the following terms.

   begin

      -- Emit the constant term if not zero:

      if Item.Const_Term /= 0
      or Item.Num_Vars    = 0
      then

         Put_Line (Solver,
              Blanks (Indent)
            & Add_Op
            & Image (Item.Const_Term));

         Add_Op(1) := '+';

      end if;

      -- Emit the variable terms:

      for V in 1 .. Item.Num_Vars loop

         Put_Line (Solver,
              Blanks (Indent)
            & Add_Op
            & Image (Item.Var(V)));

         Add_Op(1) := '+';

      end loop;

   end Put;


   procedure Objective (
      Solver : in out Solver_T;
      Func   : in     Expression_T;
      Goal   : in     Goal_T)
   is
   begin

      case Goal is

         when Minimize =>
            Put_Line (Solver, "min:");

         when Maximize =>
            Put_Line (Solver, "max:");

      end case;

      Put (
         Solver => Solver,
         Item   => Func,
         Indent => 3);

      Put_Line (Solver, ";");
      Put_Line (Solver, "");

   end Objective;


   procedure Declare_Integer_Variables (
      Solver    : in out Solver_T;
      Variables : in     Var_List_T)
   is
   begin

      Put_Line (Solver, "Int");

      for V in Variables'Range loop

         Put_Line (Solver, "  " & Image (Variables(V)));

      end loop;

      Put_Line (Solver, ";");
      Put_Line (Solver, "");

   end Declare_Integer_Variables;


   function Image (Item : Relation_T) return String
   --
   -- The LP_Solve relation symbol.
   --
   is
   begin

      case Item is
         when Equal            => return " = ";
         when Less_Or_Equal    => return " <= ";
         when Greater_Or_Equal => return " >= ";
      end case;

   end Image;


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Expression_T;
      Rel    : in     Relation_T;
      Right  : in     Expression_T)
   is
   begin

      Put      (Solver, Left);
      Put_Line (Solver, Image (Rel));
      Put      (Solver, Right, Indent => 3);

      Put_Line (Solver, ";");
      Put_Line (Solver, "");

   end Bound;


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Number_T;
      Rel    : in     Relation_T;
      Right  : in     Expression_T)
   is
   begin

      Put_Line (Solver, Image (Left) & Image (Rel));
      Put      (Solver, Right, Indent => 3);

      Put_Line (Solver, ";");
      Put_Line (Solver, "");

   end Bound;


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Expression_T;
      Rel    : in     Relation_T;
      Right  : in     Number_T)
   is
   begin

      Bound (
         Solver => Solver,
         Left   => Right,
         Rel    => Reflected(Rel),
         Right  => Left);

   end Bound;


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Var_Term_T;
      Rel    : in     Relation_T;
      Right  : in     Expression_T)
   is
   begin

      Put_Line (Solver, Image (Left) & Image (Rel));
      Put      (Solver, Right, Indent => 3);

      Put_Line (Solver, ";");
      Put_Line (Solver, "");

   end Bound;


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Expression_T;
      Rel    : in     Relation_T;
      Right  : in     Var_Term_T)
   is
   begin

      Bound (
         Solver => Solver,
         Left   => Right,
         Rel    => Reflected(Rel),
         Right  => Left);

   end Bound;


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Var_Term_T;
      Rel    : in     Relation_T;
      Right  : in     Var_Term_T)
   is
   begin

      Put_Line (Solver,
           Image (Left)
         & Image (Rel)
         & Image (Right)
         & ';');

      Put_Line (Solver, "");

   end Bound;


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Var_T;
      Rel    : in     Relation_T;
      Right  : in     Number_T)
   is
   begin

      Put_Line (Solver,
           Image (Left)
         & Image (Rel)
         & Image (Right)
         & ';');

      Put_Line (Solver, "");

   end Bound;



   --
   -- Procedures for handling an ILP problem.
   --


   function Start return Solver_T
   is

      Lp_Id : constant String := Image (Number_T (Next_Lp_Id));
      -- The numeric identifier of this Solver instance.


      function Log_File (Prefix : Options.Strings.Option_T)
      return String
      --
      -- The name for the input or output log-file for the next
      -- Solver, with the given prefix, or the null string if
      -- files are not kept.
      --
      is
      begin

         if Opt.Keep_Files then

            return Options.Strings.Value_Of (Prefix) & '_' & Lp_Id;

         else

            return "";

         end if;

      end Log_File;


   begin  -- Start

      Next_Lp_Id := Next_Lp_Id + 1;

      return Solver_T (
         ILP.Platform.Start (
            Program    => Options.Strings.Value_Of (ILP.Opt.Prog_LP_SOLVE),
            Input_Log  => Log_File (ILP.Opt.LP_Input_File ),
            Output_Log => Log_File (ILP.Opt.LP_Output_File)));

   end Start;


   procedure Stop (Solver : in out Solver_T)
   is

      Status : Exec_Cmd.Status_T;
      -- Exit status from the ILP solver.

      Code : Exec_Cmd.Exit_Code_T;
      -- Exit code from normal exit of ILP solver.

   begin

      End_Execution (
         Exec   => Solver,
         Status => Status);

      if not Exec_Cmd.Normal_Exit (Status) then

         Output.Fault (
            Location => "ILP.Stop",
            Text     =>
                 "Problem with lp_solve, exit status "
               & Exec_Cmd.Status_T'Image (Status) );

      else

         Code := Exec_Cmd.Exit_Code (Status);

         case Code is

         when Exit_Codes.Optimal =>

            null;

         when Exit_Codes.Infeasible =>

            Output.Warning ("Infeasible execution constraints.");

         when Exit_Codes.Unbounded =>

            Output.Error ("Not enough execution constraints.");

         when others =>

            Output.Fault (
               Location => "ILP.Stop",
               Text     =>
                    "Problem with lp_solve, exit code"
                  & Exec_Cmd.Exit_Code_T'Image (Code));

         end case;

      end if;

      -- Parse_Results outputs all of lp_solve output
      -- if there is an error.

   end Stop;


   function Is_Prefix (
      Whole  : Ada.Strings.Unbounded.Unbounded_String;
      Prefix : String)
   return Boolean
   --
   -- Whether the Whole string starts with the Prefix string.
   --
   is
      use Ada.Strings.Unbounded;
   begin

      return   Length(Whole) >= Prefix'Length
      and then Slice (Whole, 1, Prefix'Length) = Prefix;

   end Is_Prefix;


   Number_Error : exception;
   --
   -- A number from the ILP solver is too large or otherwise
   -- impossible to use.


   function Number (Item : in String) return Number_T
   --
   -- A number from the ILP solver, hopefully a decimal integer.
   -- Propagates Number_Error if Item does not define a useful value.
   --
   is

      Value : Number_T;
      -- The integer number displayed in Item.

      Real_Value : Long_Float;
      -- The non-integer number displayed in Item.

   begin

      -- Normally it will be an integral number:

      begin

         Value := Number_T'Value (Item);

         return Value;

      exception

      when Constraint_Error =>
         -- Oops. Not an integer, or too large.

         null;

      end;

      -- Perhaps a real (non-integral) value?

      Real_Value := Long_Float'Value (Item);

      -- Yes. Drat.

      Value := Number_T (Real_Value);
      -- This can also raise Constraint_Error, if Real_Value
      -- is larger than can fit in a Number_T.

      Output.Error (
           "ILP result """
         & Item
         & """ is not integral. Rounded to"
         & Number_T'Image (Value)
         & '.');

      return Value;

   exception

   when Constraint_Error =>

      Output.Error (
           "ILP result """
         & Item
         & """ is not integral and/or too large.");

      raise Number_Error;

   end Number;


   procedure Solve (
      Solver  : in out Solver_T;
      Results :    out Results_T)
   --
   -- Triggers the ILP solver to solve the ILP probem and
   -- parses the results. The results are:
   --
   -- > The extremal value of the objective function.
   -- > The value of each variable, giving a valuation that
   --   achieves the extremal objective value.
   --
   -- If there is a syntax error in the output of the solver, Solve
   -- dumps the rest of the output to standard error.
   --
   -- The output syntax expected is as follows, where the quote marks
   -- are only for display here, and do not occur in the output:
   --
   -- "Value of objective function: 193"
   -- "E1                   1"
   -- "E2                   1"
   -- "N1                   1"
   -- "N2                   2"
   --
   -- and so on. The order of the variables (Ei, Ni) is not significant.
   -- The number of blanks between the variable name and its value is
   -- not significant.
   --
   is
      use Ada.Strings.Unbounded;

      Value_Header : constant String := "Value of objective function:";
      -- This is what lp_solve writes out first when it has
      -- completed the program analysis.

      Infeasible_Error : constant String := "This problem is infeasible";
      -- This is what lp_solve writes out first (and as the only output)
      -- when it finds that the ILP problem is infeasible (over-constrained).

      Unbounded_Error : constant String := "This problem is unbounded";
      -- This is what lp_solve writes out first (and as the only output)
      -- when it finds that the ILP problem is not bounded.

      S : Unbounded_String;
      -- One line of output from the solver.

      Syntax_Error : exception;
      -- When some lp_solve output is not understood.


      function Variable (Name : String) return Var_T
      --
      -- Parses a variable name, consisting of a prefix letter which
      -- is 'N', 'E', 'S', 'D' or target-specific, and the index of
      -- the node, edge, step, step-edge or target-specific entity,
      -- respectively. For a 'T' = target-specific entity, there is
      -- also a target-defined prefix character before the index.
      --
      -- Raises Syntax_Error if the name has an unexpected form.
      --
      is

         Var : Var_T;
         -- The result.


         function Index (Skip : Positive) return Natural
         --
         -- The Name with Skip leading characters skipped, interpreted
         -- as a decimal integer.
         --
         is
         begin

            return Natural'Value (Name(Name'First + Skip .. Name'Last));

         end Index;


      begin  -- Variable

         case Name(Name'First) is

            when Node_Mark =>

               Var := (
                  Kind       => Node,
                  Node_Index => Flow.Node_Index_T (Index (1)));

            when Edge_Mark =>

               Var := (
                  Kind       => Edge,
                  Edge_Index => Flow.Edge_Index_T (Index (1)));

            when Step_Mark =>

               Var := (
                  Kind       => Step,
                  Step_Index => Flow.Step_Index_T (Index (1)));

            when Step_Edge_Mark =>

               Var := (
                  Kind            => Step_Edge,
                  Step_Edge_Index => Flow.Step_Edge_Index_T (Index (1)));

            when Target_Mark =>

               Var := (
                  Kind   => Target,
                  Prefix => Name(Name'First + 1),
                  Index  => Index (2));

            when others =>

               raise Constraint_Error;

         end case;

         return Var;

      exception

         when Constraint_Error =>

            Output.Fault (
               Location => "ILP.Solve.Variable",
               Text =>
                    "Variable from lp_solve has wrong form: "
                  & Name);

            raise Syntax_Error;

      end Variable;


      procedure Skip_Rest
      --
      -- Reports the Solver output that is not understood.
      -- The first problematic output line is in S.
      -- This line, and the rest of the Solver output, are copied
      -- to standard error, unless the problem is recognized as
      -- one that will be reported in some other way.
      --
      is
         use Ada.Strings.Unbounded;

         Self : constant String := "ILP.Solve.Skip_Rest";

      begin

         Output.Fault (
            Location => Self,
            Text     => "Unexpected results from ILP solving.");

         Output.Fault (
            Location => Self,
            Text     => "Rest of output from lp_solve follows.");

         loop

            Output.Fault (
               Location => Self,
               Text     => To_String(S));

            Read_Line (Solver, S);

            exit when Is_EOF (Solver);

         end loop;

      end Skip_Rest;


      Num_Vars : Natural := 0;
      -- The number of variable values retrieved from the Solver
      -- output and stored in the Results (if there is room).

      Var : Var_T;
      -- A variable, identified in the Solver output.

      Value : Number_T;
      -- The value of Var, as stated in the Solver output.

      First_Non_Alpha : Natural;
      -- The index, in a "variable value" line, of the first character
      -- that is not alphanumeric. This will be the first blank.


   begin  -- Solve

      Results.Status := Solved;
      -- Optimistic initial value can be changed later.

      -- Indicate end of ILP problem definition:

      Write_End (Solver);

      -- The first output line contains the extremal value, or
      -- says that the problem is unbounded:

      Get_Line (Solver, S);

      if Is_Prefix (Whole => S, Prefix => Infeasible_Error) then

         Results.Status := Infeasible;

      elsif Is_Prefix (Whole => S, Prefix => Unbounded_Error) then

         Results.Status := Unbounded;

      else

         if not Is_Prefix (Whole => S, Prefix => Value_Header) then

            raise Syntax_Error;

         end if;

         Delete (S, 1, Value_Header'Length);

         Results.Function_Value := Number (Trim_Left (To_String (S)));

         -- The rest of the lines contain the variable values:

         loop

            Get_Line (Solver, S);

            exit when Is_EOF (Solver);

            if Length(S) > 0 then

               First_Non_Alpha :=
                  Index (
                     Source => S,
                     Set    => Ada.Strings.Maps.Constants.Alphanumeric_Set,
                     Test   => Ada.Strings.Outside);

               Var := Variable (Slice (S, 1, First_Non_Alpha));

               Value := Number (Trim_Left (
                  Slice(S, First_Non_Alpha, Length(S))));

               Num_Vars := Num_Vars + 1;

               if Num_Vars <= Results.Num_Vars then
                  -- Still room for this variable.

                  Results.Variable_Values(Num_Vars) := (
                     Value => Value,
                     Var   => Var);

               elsif Results.Status = Solved then
                 -- Solver produces too many variable values.
                 -- Report the first excessive variable as a fault.

                 Output.Fault (
                    Location => "ILP.Solve",
                    Text     =>
                       "Too many variables from lp_solve.");

                 Results.Status := Failed;

               end if;

            end if;

         end loop;

         -- Check the number of defined variables:

         if Num_Vars /= Results.Num_Vars then

            Output.Fault (
               Location => "ILP.Solve",
               Text     =>
                    "Got"
                  & Natural'Image (Num_Vars)
                  & " variables from lp_solve, expected"
                  & Natural'Image (Results.Num_Vars));

            Results.Status := Failed;

         end if;

      end if;

   exception

   when Syntax_Error =>

      Skip_Rest;

      Results.Status := Failed;

   when Number_Error =>

      Results.Status := Failed;

   end Solve;


end ILP;
