-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  5-SEP-1990 by Mats Weber.


separate (Linear_Programming)

procedure Change_Bounds (On_Program   : in out Program;
                         Delta_Bounds : in Vector) is

   The_Delta_Bounds : Vector(Delta_Bounds'Range);

   function Is_Slack (The_Variable : Variable_Index) return Boolean is
   begin
      return The_Variable < 0;
   end;

   pragma Inline(Is_Slack);


begin
   Check_Program(On_Program);
   if Delta_Bounds'First /= On_Program.First_Row_1 or
      Delta_Bounds'Last /= On_Program.Last_Constraint
   then
      raise Bound_Mismatch;
   end if;
   -- copy Delta_Bounds into The_Delta_Bounds, adjusting the signs
   for K in Delta_Bounds'Range loop
      case On_Program.Bound_Variables_Kind(K) is
         when Slack =>
            The_Delta_Bounds(K) := Delta_Bounds(K);
         when Slack_Sign_Changed =>
            The_Delta_Bounds(K) := -Delta_Bounds(K);
         when Artificial =>
            if abs Delta_Bounds(K) > Epsilon then
               raise Invalid_Change_Of_Bounds;
            end if;
      end case;
   end loop;
   case On_Program.State is
      when Do_Dual | Do_Dual_And_Two_Phase =>
         null;
      when Do_Primal | Do_Two_Phase =>
         raise Program_In_Wrong_State;
      when Solved | Simplex_Unbounded | Not_Solvable =>
         On_Program.State := Do_Dual;
         On_Program.Pivots_Phase_1 := 0;
         On_Program.Pivots_Phase_2 := 0;
         On_Program.Pivots_Dual := 0;
   end case;
   for I in On_Program.Simplex_Table'Range(1) loop
      if I >= On_Program.First_Row_1 and then
         Is_Slack(On_Program.Basis_Variables(I))
      then
         On_Program.Simplex_Table(I, On_Program.First_Column) :=
            On_Program.Simplex_Table(I, On_Program.First_Column) +
            The_Delta_Bounds(-On_Program.Basis_Variables(I));
      end if;
      for J in On_Program.Nonbasis_Variables'Range loop
         if Is_Slack(On_Program.Nonbasis_Variables(J)) then
            On_Program.Simplex_Table(I, On_Program.First_Column) :=
               On_Program.Simplex_Table(I, On_Program.First_Column) +
               On_Program.Simplex_Table(I, J) *
               The_Delta_Bounds(-On_Program.Nonbasis_Variables(J));
         end if;
      end loop;
   end loop;
end Change_Bounds;
