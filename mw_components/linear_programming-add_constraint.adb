-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  5-SEP-1990 by Mats Weber.


with Min_Max_Functions;

separate (Linear_Programming)

procedure Add_Constraint (To_Program     : in out Program;
                          New_Constraint : in Vector;
                          New_Inequality : in Inequality;
                          New_Bound      : in Real) is

   New_First_Row : Integer;

begin
   Check_Program(To_Program);
   if New_Constraint'First /= To_Program.First_Column_1 or
      New_Constraint'Last /= To_Program.Last_Variable
   then
      raise Bound_Mismatch;
   end if;
   if To_Program.First_Row_1 - To_Program.First_Row = 1 and New_Inequality = '=' then
      -- W_Row must be added
      New_First_Row := To_Program.First_Row - 1;
   else
      New_First_Row := To_Program.First_Row;
   end if;
   case To_Program.State is
      when Do_Two_Phase | Do_Primal | Not_Solvable =>
         raise Program_In_Wrong_State;
      when Do_Dual | Solved | Simplex_Unbounded =>
         if To_Program.First_Row_1 - To_Program.First_Row /= 1 then
            raise Internal_Error;
         end if;
      when Do_Dual_And_Two_Phase =>
         if To_Program.First_Row_1 - To_Program.First_Row /= 2 then
            raise Internal_Error;
         end if;
   end case;
   declare

      New_Program : constant Program :=
                    new Program_Record(First_Row       => New_First_Row,
                                       First_Row_1     => To_Program.First_Row_1,
                                       Last_Row        => To_Program.Last_Row + 1,
                                       Last_Constraint => To_Program.Last_Constraint + 1,
                                       First_Column    => To_Program.First_Column,
                                       First_Column_1  => To_Program.First_Column_1,
                                       Last_Column     => To_Program.Last_Column);

      The_Constraint  : Vector(New_Constraint'Range);
      The_Bound       : Real;

      function Max is new Min_Max_Functions.Array_Maximum(Index      => Positive,
                                                          Item       => Variable_Index,
                                                          Item_Array => Variable_Index_Vector);

      New_Variable_Index  : Variable_Index;
      New_Variable_Kind   : Variable_Kind;

   begin
      if New_Inequality = '>' then
         -- change the sign in order to have a '<' constraint
         The_Bound := -New_Bound;
         for L in The_Constraint'Range loop
            The_Constraint(L) := -New_Constraint(L);
         end loop;
      else
         The_Bound := New_Bound;
         The_Constraint := New_Constraint;
      end if;
      if New_Inequality = '=' then
         -- artificial variable
         New_Variable_Index := Max(To_Program.Basis_Variables &
                                   To_Program.Nonbasis_Variables) + 1;
         New_Variable_Kind := Artificial;
      else
         -- slack variable
         New_Variable_Index := -New_Program.Last_Row;
         if New_Inequality = '<' then
            New_Variable_Kind := Slack;
         else
            New_Variable_Kind := Slack_Sign_Changed;
         end if;
      end if;
      if New_First_Row < To_Program.First_Row then
         -- W_Row has just been added and must be initialized to zero
         for J in To_Program.Simplex_Table'Range(2) loop
            New_Program.Simplex_Table(New_First_Row, J) := 0.0;
         end loop;
      end if;
      for I in To_Program.Simplex_Table'Range(1) loop
         for J in To_Program.Simplex_Table'Range(2) loop
            New_Program.Simplex_Table(I, J) := To_Program.Simplex_Table(I, J);
         end loop;
      end loop;
      New_Program.Basis_Variables := To_Program.Basis_Variables & New_Variable_Index;
      New_Program.Nonbasis_Variables := To_Program.Nonbasis_Variables;
      New_Program.Bound_Variables_Kind := To_Program.Bound_Variables_Kind & New_Variable_Kind;
      New_Program.Last_Variable := To_Program.Last_Variable;
      if To_Program.State = Do_Dual_And_Two_Phase or New_Inequality = '=' then
         New_Program.State := Do_Dual_And_Two_Phase;
      else
         New_Program.State := Do_Dual;
      end if;
      New_Program.Pivots_Phase_1 := 0;
      New_Program.Pivots_Phase_2 := 0;
      New_Program.Pivots_Dual := 0;
      for J in New_Program.Simplex_Table'Range(2) loop
         if J = New_Program.First_Column then
            New_Program.Simplex_Table(New_Program.Last_Row, J) := The_Bound;
         elsif To_Program.Nonbasis_Variables(J) in The_Constraint'Range then
            New_Program.Simplex_Table(New_Program.Last_Row, J) :=
               The_Constraint(To_Program.Nonbasis_Variables(J));
         else
            New_Program.Simplex_Table(New_Program.Last_Row, J) := 0.0;
         end if;
         for I in To_Program.Basis_Variables'Range loop
            if To_Program.Basis_Variables(I) in The_Constraint'Range then
               New_Program.Simplex_Table(New_Program.Last_Row, J) :=
                  New_Program.Simplex_Table(New_Program.Last_Row, J) -
                  The_Constraint(To_Program.Basis_Variables(I)) *
                  To_Program.Simplex_Table(I, J);
            end if;
         end loop;
         -- add the new row to W_Row if present and if the new constraint is '='
         if New_Inequality = '=' then
            New_Program.Simplex_Table(New_First_Row, J) :=
               New_Program.Simplex_Table(New_First_Row, J) +
               New_Program.Simplex_Table(New_Program.Last_Row, J);
         end if;
      end loop;
      Destroy(To_Program);
      To_Program := New_Program;
   end;
end Add_Constraint;
