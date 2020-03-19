-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  5-SEP-1990 by Mats Weber.


separate (Linear_Programming)

procedure Create_Program (New_Program  : in out Program;
                          Objective    : in Vector;
                          Constraints  : in Matrix;
                          Inequalities : in Inequality_Vector;
                          Bounds       : in Vector) is

   Artificial_And_Slack : array (Inequalities'Range) of Boolean;
      -- True for each constraint that needs both a slack variable
      -- and an artificial variable.
      -- One additional column is added for each such constraint.

   Additional_Columns   : Natural := 0;
      -- Card(Artificial_And_Slack)

begin
   if Objective'First /= Constraints'First(2) or Objective'Last /= Constraints'Last(2) or
      Constraints'First(1) /= Inequalities'First or Constraints'Last(1) /= Inequalities'Last or
      Inequalities'First /= Bounds'First or Inequalities'Last /= Bounds'Last
   then
      raise Bound_Mismatch;
   end if;
   for I in Inequalities'Range loop
      if (Inequalities(I) = '<' and Bounds(I) < -Epsilon) or
         (Inequalities(I) = '>' and Bounds(I) > Epsilon)
      then
         Artificial_And_Slack(I) := True;
         Additional_Columns := Additional_Columns + 1;
      else
         Artificial_And_Slack(I) := False;
      end if;
   end loop;
   --
   Destroy(New_Program);
   New_Program := new Program_Record(First_Row       => Constraints'First(1) - 2,
                                     First_Row_1     => Constraints'First(1),
                                     Last_Row |
                                     Last_Constraint => Constraints'Last(1),
                                     First_Column    => Constraints'First(2) - 1,
                                     First_Column_1  => Constraints'First(2),
                                     Last_Column     => Constraints'Last(2) + Additional_Columns);
   declare

      The_Program : Program_Record renames New_Program.all;

      subtype Row_Index   is Integer range The_Program.Simplex_Table'Range(1);
      subtype Row_Index_0 is Row_Index range The_Program.First_Row_1 - 1..Row_Index'Last;
      subtype Row_Index_1 is Row_Index_0 range The_Program.First_Row_1..Row_Index'Last;

      subtype Column_Index   is Natural range The_Program.Simplex_Table'Range(2);
      subtype Column_Index_1 is Column_Index range The_Program.First_Column_1..Column_Index'Last;

      W_Row      : constant Row_Index := Row_Index'First;
      Z_Row      : constant Row_Index := Row_Index_0'First;
      B_Column   : constant Column_Index := Column_Index'First;

      A          : Simplex_Matrix renames The_Program.Simplex_Table;

      subtype Variable_Index is Linear_Programming.Variable_Index
                                range -Bounds'Last..Objective'Last + Constraints'Length(1);
         -- used for numbering variables:
         --    actual variables are numbered Objective'First..Objective'Last,
         --    slack variables are numbered -Bounds'First..-Bounds'Last in decreasing order
         --                                 (with unused positions at '=' constraints),
         --    artificial variables are numbered Objective'Last + 1..
         --                                      Objective'Last + <number of artificial variables>

      Basis_Variables       : Variable_Index_Vector renames The_Program.Basis_Variables;
      Nonbasis_Variables    : Variable_Index_Vector renames The_Program.Nonbasis_Variables;
         -- used for tracking the locations of variables

      Bound_Variables_Kind  : Variable_Kind_Vector renames The_Program.Bound_Variables_Kind;
         -- used to remember which slack variables had their sign changed
         -- and which ones are artificial (this information is used by Change_Bounds)


      Last_Artificial_Variable   : Variable_Index := Objective'Last;
      Last_Slack_Column          : Column_Index   := Objective'Last;
      Sign_Changed,
      Artificial_Variable_Added  : Boolean;

   begin
      for J in Column_Index'First..Objective'Last loop
         A(W_Row, J) := 0.0;
      end loop;
      A(Z_Row, B_Column) := 0.0;
      for J in Objective'Range loop
         A(Z_Row, J) := -Objective(J);
      end loop;
      for J in Objective'Range loop
         Nonbasis_Variables(J) := J;
      end loop;
      for I in Row_Index_1 loop
         Sign_Changed := Bounds(I) < 0.0;
         if Inequalities(I) = '=' then
            Bound_Variables_Kind(I) := Artificial;
         else
            if Sign_Changed then
               Bound_Variables_Kind(I) := Slack_Sign_Changed;
            else
               Bound_Variables_Kind(I) := Slack;
            end if;
         end if;
         if Sign_Changed then
            A(I, B_Column) := -Bounds(I);
            for J in Constraints'Range(2) loop
               A(I, J) := -Constraints(I, J);
            end loop;
         else
            A(I, B_Column) := Bounds(I);
            for J in Constraints'Range(2) loop
               A(I, J) := Constraints(I, J);
            end loop;
         end if;
         for J in Objective'Last + 1..Column_Index'Last loop
            A(I, J) := 0.0;
         end loop;
         if Artificial_And_Slack(I) then
            -- add one slack variable (out of basis) and one aritificial variable (in basis)
            Last_Slack_Column := Last_Slack_Column + 1;
            A(I, Last_Slack_Column) := -1.0;
            A(W_Row, Last_Slack_Column) := -1.0;
            A(Z_Row, Last_Slack_Column) := 0.0;
            Nonbasis_Variables(Last_Slack_Column) := -I;
            Last_Artificial_Variable := Last_Artificial_Variable + 1;
            Basis_Variables(I) := Last_Artificial_Variable;
            Artificial_Variable_Added := True;
         elsif Inequalities(I) = '=' then
            -- add an artificial variable
            Last_Artificial_Variable := Last_Artificial_Variable + 1;
            Basis_Variables(I) := Last_Artificial_Variable;
            Artificial_Variable_Added := True;
         else
            -- add a slack variable
            Basis_Variables(I) := -I;
            Artificial_Variable_Added := False;
         end if;
         if Artificial_Variable_Added then
            for J in Column_Index'First..Objective'Last loop
               A(W_Row, J) := A(W_Row, J) + A(I, J);
            end loop;
         end if;
      end loop;
      --
      if Last_Slack_Column /= Column_Index'Last then
         raise Internal_Error;
      end if;
      The_Program.Last_Variable := Objective'Last;
      The_Program.State := Do_Two_Phase;
      The_Program.Pivots_Phase_1 := 0;
      The_Program.Pivots_Phase_2 := 0;
      The_Program.Pivots_Dual := 0;
   end;
end Create_Program;
