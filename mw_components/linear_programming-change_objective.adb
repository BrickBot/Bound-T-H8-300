-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  5-SEP-1990 by Mats Weber.


separate (Linear_Programming)

procedure Change_Objective (On_Program    : in out Program;
                            New_Objective : in Vector) is
begin
   Check_Program(On_Program);
   if New_Objective'First /= On_Program.First_Column_1 or
      New_Objective'Last /= On_Program.Last_Variable
   then
      raise Bound_Mismatch;
   end if;
   case On_Program.State is
      when Do_Dual | Do_Dual_And_Two_Phase | Not_Solvable =>
         raise Program_In_Wrong_State;
      when Do_Primal | Do_Two_Phase =>
         null;
      when Solved | Simplex_Unbounded =>
         On_Program.State := Do_Primal;
         On_Program.Pivots_Phase_1 := 0;
         On_Program.Pivots_Phase_2 := 0;
         On_Program.Pivots_Dual := 0;
   end case;
   for J in On_Program.Simplex_Table'Range(2) loop
      if J >= On_Program.First_Column_1 and then
         On_Program.Nonbasis_Variables(J) in New_Objective'Range
      then
         On_Program.Simplex_Table(On_Program.First_Row, J) :=
            -New_Objective(On_Program.Nonbasis_Variables(J));
      else
         On_Program.Simplex_Table(On_Program.First_Row, J) := 0.0;
      end if;
      for I in On_Program.Basis_Variables'Range loop
         if On_Program.Basis_Variables(I) in New_Objective'Range then
            On_Program.Simplex_Table(On_Program.First_Row, J) :=
               On_Program.Simplex_Table(On_Program.First_Row, J) +
               On_Program.Simplex_Table(I, J) *
               New_Objective(On_Program.Basis_Variables(I));
         end if;
      end loop;
   end loop;
end Change_Objective;
