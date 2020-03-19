-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  1-APR-1990 by Mats Weber.


with Exchange,
     Unchecked_Deallocation;

package body Linear_Programming is
-------------------------------

   type Simplex_Matrix is array (Integer range <>, Natural range <>) of Real;

   subtype Variable_Index is Integer;

   type Variable_Index_Vector is array (Positive range <>) of Variable_Index;

   type Program_State is (Do_Two_Phase,
                          Do_Primal,
                          Do_Dual,
                          Do_Dual_And_Two_Phase,
                          Solved,
                          Not_Solvable,
                          Simplex_Unbounded);

   type Variable_Kind is (Slack, Slack_Sign_Changed, Artificial);

   type Variable_Kind_Vector is array (Positive range <>) of Variable_Kind;


   type Program_Record (First_Row       : Integer;
                        First_Row_1     : Positive;
                        Last_Row,
                        Last_Constraint : Natural;
                        First_Column    : Natural;
                        First_Column_1  : Positive;
                        Last_Column     : Natural) is
      record
         Simplex_Table        : Simplex_Matrix(First_Row..Last_Row, First_Column..Last_Column);
         Basis_Variables      : Variable_Index_Vector(First_Row_1..Last_Row);
         Nonbasis_Variables   : Variable_Index_Vector(First_Column_1..Last_Column);
         Bound_Variables_Kind : Variable_Kind_Vector(First_Row_1..Last_Constraint);
         Last_Variable        : Natural;
         State                : Program_State;
         Pivots_Phase_1,
         Pivots_Phase_2,
         Pivots_Dual          : Natural;
      end record;


   Internal_Error : exception;   -- should never be raised


   procedure Solve_Program (Objective    : in Vector;
                            Constraints  : in Matrix;
                            Inequalities : in Inequality_Vector;
                            Bounds       : in Vector;
                            Variables    : out Vector;
                            Value        : out Real) is

      Pivots_1, Pivots_2 : Natural;

   begin
      Solve_Program(Objective,
                    Constraints,
                    Inequalities,
                    Bounds,
                    Variables,
                    Value,
                    Phase_1_Pivots => Pivots_1,
                    Phase_2_Pivots => Pivots_2);
   end Solve_Program;


   procedure Solve_Program (Objective       : in Vector;
                            Constraints     : in Matrix;
                            Inequalities    : in Inequality_Vector;
                            Bounds          : in Vector;
                            Variables       : out Vector;
                            Value           : out Real;
                            Phase_1_Pivots,
                            Phase_2_Pivots  : out Natural) is

      The_Program : Program;

   begin
      if Variables'First /= Constraints'First(2) or Variables'Last /= Constraints'Last(2) then
         raise Bound_Mismatch;
      end if;
      Create_Program(New_Program  => The_Program,
                     Objective    => Objective,
                     Constraints  => Constraints,
                     Inequalities => Inequalities,
                     Bounds       => Bounds);
      Solve(The_Program);
      Variables := Linear_Programming.Variables(The_Program);
      Value := Linear_Programming.Value(The_Program);
      Phase_1_Pivots := Linear_Programming.Phase_1_Pivots(The_Program);
      Phase_2_Pivots := Linear_Programming.Phase_2_Pivots(The_Program);
      Destroy(The_Program);
   exception
      when No_Solution | Unbounded_Simplex =>
         Destroy(The_Program);
         raise;
   end Solve_Program;


   procedure Check_Program (The_Program : in Program) is
   begin
      if The_Program = null then
         raise Program_Is_Null;
      end if;
   end Check_Program;

   pragma Inline(Check_Program);


   procedure Create_Program (New_Program  : in out Program;
                             Objective    : in Vector;
                             Constraints  : in Matrix;
                             Inequalities : in Inequality_Vector;
                             Bounds       : in Vector) is separate;


   procedure Solve (The_Program : in out Program) is separate;


   procedure Add_Constraint (To_Program     : in out Program;
                             New_Constraint : in Vector;
                             New_Inequality : in Inequality;
                             New_Bound      : in Real) is separate;


   procedure Change_Objective (On_Program    : in out Program;
                               New_Objective : in Vector) is separate;


   procedure Change_Bounds (On_Program   : in out Program;
                            Delta_Bounds : in Vector) is separate;


   function Variables (Of_Program : Program) return Vector is

      The_Variables  : Vector(Of_Program.First_Column_1..Of_Program.Last_Variable);
      Variables_Set  : array (The_Variables'Range) of Boolean := (others => False);

   begin
      Check_Program(Of_Program);
      case Of_Program.State is
         when Do_Two_Phase | Do_Primal | Do_Dual | Do_Dual_And_Two_Phase =>
            raise Program_Not_Solved;
         when Solved =>
            null;
         when Not_Solvable =>
            raise No_Solution;
         when Simplex_Unbounded =>
            raise Unbounded_Simplex;
      end case;
      for I in Of_Program.Basis_Variables'Range loop
         if Of_Program.Basis_Variables(I) in The_Variables'Range then
            The_Variables(Of_Program.Basis_Variables(I)) :=
               Of_Program.Simplex_Table(I, Of_Program.First_Column);
            Variables_Set(Of_Program.Basis_Variables(I)) := True;
         end if;
      end loop;
      for J in Of_Program.Nonbasis_Variables'Range loop
         if Of_Program.Nonbasis_Variables(J) in The_Variables'Range then
            The_Variables(Of_Program.Nonbasis_Variables(J)) := 0.0;
            Variables_Set(Of_Program.Nonbasis_Variables(J)) := True;
         end if;
      end loop;
      if Variables_Set /= (Variables_Set'Range => True) then
         raise Internal_Error;
      end if;
      return The_Variables;
   end Variables;


   function Value (Of_Program : Program) return Real is
   begin
      Check_Program(Of_Program);
      case Of_Program.State is
         when Do_Two_Phase | Do_Primal | Do_Dual | Do_Dual_And_Two_Phase =>
            raise Program_Not_Solved;
         when Solved =>
            null;
         when Not_Solvable =>
            raise No_Solution;
         when Simplex_Unbounded =>
            raise Unbounded_Simplex;
      end case;
      return Of_Program.Simplex_Table(Of_Program.First_Row_1 - 1, Of_Program.First_Column);
   end Value;


   function Phase_1_Pivots (On_Program : Program) return Natural is
   begin
      Check_Program(On_Program);
      return On_Program.Pivots_Phase_1;
   end;

   function Phase_2_Pivots (On_Program : Program) return Natural is
   begin
      Check_Program(On_Program);
      return On_Program.Pivots_Phase_2;
   end;

   function Dual_Pivots (On_Program : Program) return Natural is
   begin
      Check_Program(On_Program);
      return On_Program.Pivots_Dual;
   end;


   procedure Destroy (The_Program : in out Program) is

      procedure Dispose is new Unchecked_Deallocation(Program_Record,
                                                      Program);

   begin
      Dispose(The_Program);
   end Destroy;


   procedure Assign (Object : in out Program; Value : in Program) is
   begin
      Destroy(Object);
      Object := new Program_Record'(Value.all);
   end Assign;


   procedure Swap (Program_1, Program_2 : in out Program) is

      procedure Swap is new Exchange(Program);

   begin
      Swap(Program_1, Program_2);
   end Swap;

end Linear_Programming;
