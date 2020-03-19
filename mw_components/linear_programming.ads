-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  1-APR-1990 by Mats Weber.


generic
   type Real is digits <>;
   Epsilon : in Real;
      -- bound used when comparing Reals
package Linear_Programming is
--------------------------

   -- This package can be used in two ways:
   --    > Through the subprograms Solve_Program, permitting only
   --      to solve linear programs and obtain their solutions.
   --    > Using the type Program and other operations, enabling
   --      more general manipulations on linear programs without
   --      solving similar programs over and over.
   --      The possibilities thus offered are:
   --         - adding constraints
   --         - changing the objective function
   --         - changing the bounds of the constraints.


   type Vector is array (Positive range <>) of Real;
   type Matrix is array (Positive range <>,
                         Positive range <>) of Real;

   type Inequality is ('<', '=', '>');    -- '<' means '<=' and '>' means '>='

   type Inequality_Vector is array (Positive range <>) of Inequality;


   procedure Solve_Program (Objective    : in Vector;
                            Constraints  : in Matrix;
                            Inequalities : in Inequality_Vector;
                            Bounds       : in Vector;
                            Variables    : out Vector;
                            Value        : out Real);

   procedure Solve_Program (Objective       : in Vector;
                            Constraints     : in Matrix;
                            Inequalities    : in Inequality_Vector;
                            Bounds          : in Vector;
                            Variables       : out Vector;
                            Value           : out Real;
                            Phase_1_Pivots,
                            Phase_2_Pivots  : out Natural);
      -- Solves the linear program
      --    Maximize    Value = Objective * Variables
      --    Subject to  Constraints * Variables {<=, =, >=} Bounds
      --                Variables >= 0.0
      -- using the two-phase simplex method.
      -- Raises Bound_Mismatch if the parameters do not have
      -- matching bounds.
      -- Raises Unbounded_Simplex if Objective * Variables can
      -- increase indefinitely without violating the constraints.
      -- Raises No_Solution if the constraints cannot be satisfied.


   type Program is limited private;
      -- Type representing a linear program

   Null_Program : constant Program;
      -- Default initial value of objects of type Program.
      -- All subprograms below except Assign, Destroy and Swap
      -- will raise Program_Is_Null if they are given a Program
      -- with this value as a parameter


   procedure Create_Program (New_Program  : in out Program;
                             Objective    : in Vector;
                             Constraints  : in Matrix;
                             Inequalities : in Inequality_Vector;
                             Bounds       : in Vector);
      -- Creates a new (unsolved) program

   procedure Solve (The_Program : in out Program);
      -- Solves the given program


   procedure Add_Constraint (To_Program     : in out Program;
                             New_Constraint : in Vector;
                             New_Inequality : in Inequality;
                             New_Bound      : in Real);
      -- Adds a new constraint to the given program

   procedure Change_Objective (On_Program    : in out Program;
                               New_Objective : in Vector);
      -- Changes the objective function on the given program,
      -- replacing it with New_Objective

   procedure Change_Bounds (On_Program   : in out Program;
                            Delta_Bounds : in Vector);
      -- Changes the bounds of the constraints on the given program,
      -- replacing them with <former bounds> + Delta_Bounds.
      -- Bounds corresponding to '=' constraints cannot be changed
      -- and the corresponding Delta_Bounds must be 0.0, or
      -- Invalid_Change_Of_Bounds will be raised


   function Variables (Of_Program : Program) return Vector;
   function Value     (Of_Program : Program) return Real;
      -- Return the values of the variables and objective function
      -- in the given program

   function Phase_1_Pivots (On_Program : Program) return Natural;
   function Phase_2_Pivots (On_Program : Program) return Natural;
   function Dual_Pivots    (On_Program : Program) return Natural;
      -- Return the number of pivots performed in the last call to Solve:
      --    Phase_1_Pivots: pivots in the first primal phase
      --                    (elimination of artificial variables)
      --    Phase_2_Pivots: pivots in the second primal phase
      --                    (regular primal simplex algorithm)
      --    Dual_Pivots: pivots using the dual simplex algorithm
      --                 (only after Change_Bounds and Add_Constraint)


   procedure Destroy (The_Program : in out Program);
      -- Destroy the given program, deallocating memory

   procedure Assign (Object : in out Program; Value : in Program);
      -- Object := Value;

   procedure Swap (Program_1, Program_2 : in out Program);
      -- Exchanges Program_1 and Program_2


   Bound_Mismatch,
   Unbounded_Simplex,
   No_Solution,
   Program_Not_Solved,
   Program_In_Wrong_State,
   Invalid_Change_Of_Bounds,
   Program_Is_Null           : exception;

private

   type Program_Record (First_Row       : Integer;
                        First_Row_1     : Positive;
                        Last_Row,
                        Last_Constraint : Natural;
                        First_Column    : Natural;
                        First_Column_1  : Positive;
                        Last_Column     : Natural);

   type Program is access Program_Record;

   Null_Program : constant Program := null;


   pragma Inline(Phase_1_Pivots, Phase_2_Pivots, Dual_Pivots,
                 Destroy, Assign);

end Linear_Programming;
