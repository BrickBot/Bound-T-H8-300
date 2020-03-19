-- ILP (decl)
--
-- Defining and solving Integer Linear Programming problems where
-- the variables are associated with flow-graph elements (steps,
-- step-edges, nodes, node-edges).
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: ilp.ads,v $
-- Revision 1.6  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.5  2008-09-24 08:38:52  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.4  2007/12/17 13:54:38  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.3  2004/05/01 10:29:35  niklas
-- First Tidorum version.
-- Added support for target-specific ILP variables.
-- Added procedure to Add two Expressions.
-- Added constant Zero_Expression.
-- Added support for the Keep_Files option.
--
-- Revision 1.2  2003/03/11 08:26:42  holsti
-- Major update to make the types Var_Term_T, Expression_T and
-- Solver_T private, to provide operations on these types that
-- are easier to use and better encapsulated, and to avoid
-- using the heap.
--
-- Revision 1.1  2001/11/19 11:10:18  saarinen
-- First versions.
--


with Exec_Cmd;
with Flow;


package ILP is


   --
   --    Integer Linear Programming (ILP) problems
   --
   -- An ILP problem consists of:
   --
   -- > A set of integer variables (unknowns).
   -- > A set of affine constraints on the variables.
   -- > An affine expression (objective function) to be
   --   minimized or maximized.
   --
   -- The solution of the ILP problem is a set of variable values
   -- that gives the desired extremal value of the objective
   -- function.
   --
   -- An affine constraint is an arithmetic relation (equal, less,
   -- greater) between two affine expressions of the variables.
   -- An affine expression is a linear expression of the variables
   -- plus an optional constant term.
   --


   --
   --    Numbers
   --
   -- Integer numbers are used as constant oefficients, variable
   -- coefficients, objective function values, and solution values
   -- (variable values).
   --


   type Number_T is new Integer;
   --
   -- The computational type, should be large enough to contain
   -- the necessary values of:
   --
   -- > Execution time of a node or edge (Processor.Time_T).
   -- > Execution count of a node or edge (Flow.Execution.Count_T).
   -- > Node or edge index (Flow.Node_Index_T, Flow.Edge_Index_T).


   --
   --    Variables
   --
   -- In our application, we assume that the variables are associated
   -- with a subset of the elements of a subprogram flow-graph.
   -- Thus, a variable corresponds to a step or a node or an edge
   -- between steps or nodes. Whether the correspondence is to steps
   -- and step-edges, or nodes and node-edges, depends on the specific
   -- application of ILP analysis.
   --


   type Var_Kind_T is (Node, Step, Edge, Step_Edge, Target);
   --
   -- The kinds of variables in the ILP problems.
   --
   -- Node
   --    A variable associated with a flow-graph node.
   -- Step
   --    A variable associated with a flow-graph step.
   -- Edge
   --    A variable associated with an edge between nodes.
   -- Step_Edge
   --    A variable associated with an edge between steps.
   -- Target
   --    A target-specific variable, with a target-specific meaning.
   --
   -- One ILP problem usually contains only variables of the
   -- same "level" in the flow-graph, that is either variables
   -- of the kinds (Node, Edge) or the kinds (Step, Step_Edge).


   type Var_T (Kind : Var_Kind_T := Node) is record

      case Kind is

         when Node =>
            Node_Index : Flow.Node_Index_T;

         when Step =>
            Step_Index : Flow.Step_Index_T;

         when Edge =>
            Edge_Index : Flow.Edge_Index_T;

         when Step_Edge =>
            Step_Edge_Index : Flow.Step_Edge_Index_T;

         when Target =>
            Prefix : Character;
            Index  : Natural;

      end case;

   end record;
   --
   -- Identifies an ILP variable by means of its Kind and the
   -- corresponding Index.


   function Image (Item : Var_T) return String;
   --
   -- The LP_Solve identifier for the given variable.


   type Var_Term_T is private;
   --
   -- A term of the form "coefficient * variable", to be used
   -- as one term in an affine expression.


   function "*" (Left : Number_T; Right : Var_T)
   return Var_Term_T;
   --
   -- Combines a coefficient and a variable identifier into
   -- a variable term.


   function "*" (Left : Number_T; Right : Var_Term_T)
   return Var_Term_T;
   --
   -- Multiplies a variable term by a constant factor.


   type Var_List_T is array (Positive range <>) of Var_T;
   --
   -- A list of variables (variable IDs).


   No_Variables : Var_List_T (1 .. 0);
   --
   -- An empty variable list.


   --
   --    Affine expressions
   -- 
   -- An affine expression (in this context) is represented by
   --
   -- > A literal constant term, possibly zero.
   -- > A list of variable terms, each of which is
   --   - a variable identifier
   --   - a literal coefficient.
   --


   type Expression_T (Max_Vars : Natural) is private;
   --
   -- An affine expression containing an constant term and
   -- a set of variable terms, up to the given maximum number
   -- of variable terms.
   --
   -- The default initial value of an expression is the zero
   -- expression (constant = 0, no variable terms).


   Zero_Expression : constant Expression_T;
   --
   -- An expression with a zero value.


   Too_Many_Terms : exception;
   --
   -- Raised on an attempt to add more variable terms to an
   -- expression that already has Max_Vars terms.


   procedure Erase (Item : in out Expression_T);
   --
   -- Erases the expression, giving the zero expression.


   procedure Add (
      Const : in     Number_T;
      To    : in out Expression_T);
   --
   -- Adds a constant term Const to the expression To.
   -- If the expression already has a constant term, the new
   -- constant is added to the existing constant.


   procedure Add (
      Term : in     Var_Term_T;
      To   : in out Expression_T);
   --
   -- Adds the given Term to the expression To, assuming that the
   -- term has a non-zero coefficient.
   -- At most To.Max_Vars such terms can be added, else Too_Many_Tems
   -- is raised.
   -- Note that terms using the same variable are nevertheless
   -- counted as different terms towards this limit (they are
   -- not "collected" into one term).


   procedure Add (
      Expr : in     Expression_T;
      To   : in out Expression_T);
   --
   -- Adds the given Expr expression to the expression To, by adding
   -- in turn each term from Expr. The number of terms that can be added
   -- in this way is limited as in the operation Add (Term, To).


   function "*" (Left : Number_T; Right : Expression_T)
   return Expression_T;
   --
   -- The given expression with all terms multiplied by the
   -- given factor.


   function Sum_Of (
      Nodes : Flow.Node_List_T;
      Const : Number_T)
   return Expression_T;
   --
   -- The sum of the variables corresponding to the given
   -- nodes, plus the given constant.


   function Sum_Of (
      Edges : Flow.Edge_List_T;
      Const : Number_T)
   return Expression_T;
   --
   -- The sum of the variables corresponding to the given
   -- edges, plus the given constant.


   function Sum_Of (
      Edges : Flow.Step_Edge_List_T;
      Const : Number_T)
   return Expression_T;
   --
   -- The sum of the variables corresponding to the given
   -- step-edges, plus the given constant.



   --
   --    Defining and solving ILP problems
   --


   type Solver_T is private;
   --
   -- Refers to a software entity that can solve an ILP problem.
   -- To solve an ILP problem, the following steps must be done
   -- using subprograms defined in this package:
   --
   -- > Create and start a Solver_T object.
   -- > Define the ILP problem to the Solver object.
   -- > Ask the Solver object to solve the problem.
   -- > Stop the Solver object.
   --
   -- The steps are designed so that the Solver object can be
   -- implemented via an external program that interacts with the
   -- present program through (text) streams, but this is not the
   -- only possible implementation.


   --
   --    Defining an ILP problem
   --
   -- The ILP problem is defined by a sequence of calls of the
   -- following subprograms, applied to one and the same Solver
   -- object. Each call defines some aspect of the ILP problem:
   -- the variables, the constraints, the objective function.
   -- Before any of these operations is called, the Solver must
   -- have been Started (and must not have been Stopped).
   --
   -- The nominal sequence is a little peculiar, from a normal
   -- mathematical viewpoint, since it is:
   --   1. Define the objective function.
   --   2. Define the constraints.
   --   3. Define the variables (as integers).
   --
   -- All the following operations that take an Expression_T (or a
   -- Var_Term_T) employ pure "in" semantics in the sense that after
   -- the call, the caller can modify (reuse for other purposes) the
   -- actual expression (or var term) object. The contents of the
   -- expression object at the time of the call are transferred to
   -- the Solver.


   procedure Comment (
      Solver : in out Solver_T;
      Text   : in     String;
      Indent : in     Natural := 0);
   --
   -- Adds a comment-line to the textual definition of the ILP problem.


   type Goal_T is (Minimize, Maximize);
   --
   -- The goal of an ILP problem is to minimize or maximize
   -- an objective function.


   procedure Objective (
      Solver : in out Solver_T;
      Func   : in     Expression_T;
      Goal   : in     Goal_T);
   --
   -- Creates the objective function for the ILP problem.


   procedure Declare_Integer_Variables (
      Solver    : in out Solver_T;
      Variables : in     Var_List_T);
   --
   -- Declares all the given variables as integers.
   -- This operation is required for some ILP solvers that actually
   -- solve "mixed" real/integer programming problems.


   type Relation_T is (
      Equal,
      Less_Or_Equal,
      Greater_Or_Equal);
   --
   -- The kinds of arithmetic relations that can be used
   -- in the constraints in an ILP problem.


   Reflected : constant array (Relation_T) of Relation_T := (
      Equal            => Equal,
      Less_Or_Equal    => Greater_Or_Equal,
      Greater_Or_Equal => Less_Or_Equal);
   --
   -- The same relation but swapping left and right sides.
   --    (X rel Y) iff (Y Reflected(rel) X).


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Expression_T;
      Rel    : in     Relation_T;
      Right  : in     Expression_T);
   --
   -- Declares the constraint "Left Rel Right".


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Number_T;
      Rel    : in     Relation_T;
      Right  : in     Expression_T);
   --
   -- Declares the constraint "Left Rel Right".


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Expression_T;
      Rel    : in     Relation_T;
      Right  : in     Number_T);
   --
   -- Declares the constraint "Left Rel Right".


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Var_Term_T;
      Rel    : in     Relation_T;
      Right  : in     Expression_T);
   --
   -- Declares the constraint "Left Rel Right".


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Expression_T;
      Rel    : in     Relation_T;
      Right  : in     Var_Term_T);
   --
   -- Declares the constraint "Left Rel Right".


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Var_Term_T;
      Rel    : in     Relation_T;
      Right  : in     Var_Term_T);
   --
   -- Declares the constraint "Left Rel Right".


   procedure Bound (
      Solver : in out Solver_T;
      Left   : in     Var_T;
      Rel    : in     Relation_T;
      Right  : in     Number_T);
   --
   -- Declares the constraint "Left Rel Right".



   --
   --    Solutions
   --
   -- The solution of an ILP problem consist of:
   --
   -- > The extreme value of the objective function.
   -- > The variable values (valuation) that yields this
   --   extreme value.
   --


   type Status_T is (
      Solved,
      Infeasible,
      Unbounded,
      Failed);
   --
   -- The possible results of solving, or trying to solve an ILP problem.
   --
   -- Solved
   --    A solution was found.
   -- Infeasible
   --    The problem seems to be over-constrained; no solution was found.
   -- Unbounded
   --    The problems seems to be unbounded; no solution was found.
   -- Failed
   --    Some other type of failure occurred; no solution was found.


   type Var_Value_T is record
      Value : Number_T;
      Var   : Var_T;
   end record;
   --
   -- Holds the value of one variable.


   type Valuation_T is array (Positive range <>) of Var_Value_T;
   --
   -- Holds the value for each variable in a set.


   type Results_T (Num_Vars : Natural) is record
      Status          : Status_T;
      Function_Value  : Number_T;
      Variable_Values : Valuation_T (1 .. Num_Vars);
   end record;
   --
   -- Holds the results of one ILP problem.
   --
   -- Num_Vars
   --    The number of variables.
   -- Status
   --    Success or failure of solving the ILP problem.
   --    The remaining components are defined only for Status = Solved.
   -- Function_Value
   --    The extremal value of the objective function.
   -- Variable_Values
   --    The values of the variables that achieve the
   --    extremal function value.



   --
   -- Procedures for handling an ILP problem.
   --


   function Start return Solver_T;
   --
   -- Creates and start a Solver object.
   -- After this the problem can be presented to the solver
   -- and then solved.


   procedure Stop (Solver : in out Solver_T);
   --
   -- After this call the solver is no longer valid.


   procedure Solve (
      Solver  : in out Solver_T;
      Results :    out Results_T);
   --
   -- Requests the Solver to solve the defined ILP problem and to
   -- give the results.
   --
   -- Solver
   --    A Solver object that has been Started and for which an
   --    ILP problem has been defined using the operations
   --    Objective, Bound and Declare_Integer_Variables.
   -- Results
   --    The solution. Results.Status shows what happened.
   --
   -- After this operation has been used, the only permitted
   -- operation on the same Solver is Stop.


private


   type Solver_T is new Exec_Cmd.Execution_T;
   --
   -- The ILP solver is executed as a child process.


   type Var_Term_T is record
      Coefficient : Number_T;
      Variable    : Var_T;
   end record;
   --
   -- A variable term in an affine expression.


   type Var_Term_List_T is array (Positive range <>) of Var_Term_T;
   --
   -- A list of variable terms.


   type Expression_T (Max_Vars : Natural) is record
      Const_Term : Number_T := 0;
      Num_Vars   : Natural  := 0;
      Var        : Var_Term_List_T (1 .. Max_Vars);
   end record;
   --
   -- An affine expression of the variables in an ILP problem.
   --
   -- Const_Term
   --    The constant term.
   -- Num_Vars
   --    The number of variable terms.
   -- Var
   --    The variable terms with indices 1 .. Num_Vars.
   --
   -- The value of the expression is the Const_Term plus the
   -- sum of Var(i).Coefficient * Var(i).Variable, i = 1 .. Num_Vars.


   Zero_Expression : constant Expression_T := (
      Max_Vars   => 0,
      Const_Term => 0,
      Num_Vars   => 0,
      Var        => (others => (
         Coefficient => 0,
         Variable    => (Kind => Node, Node_Index => 1))));


end ILP;
