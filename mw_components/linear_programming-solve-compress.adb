-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  5-SEP-1990 by Mats Weber.


separate (Linear_Programming.Solve)

procedure Compress (The_Program        : in out Program;
                    New_First_Row      : in Integer;
                    Active_Rows        : in Boolean_Vector;
                    Last_Active_Row    : in Natural;
                    Active_Columns     : in Boolean_Vector;
                    Last_Active_Column : in Natural) is

   Result : constant Program :=
            new Program_Record(First_Row       => New_First_Row,
                               First_Row_1     => The_Program.First_Row_1,
                               Last_Row        => Last_Active_Row,
                               Last_Constraint => The_Program.Last_Constraint,
                               First_Column    => The_Program.First_Column,
                               First_Column_1  => The_Program.First_Column_1,
                               Last_Column     => Last_Active_Column);

   Result_Row     : Integer range Result.First_Row - 1..Result.Last_Row :=
                    Result.First_Row_1 - 1;

   Result_Column  : Integer range Result.First_Column - 1..Result.Last_Column :=
                    Result.First_Column - 1;

begin
   for I in The_Program.Basis_Variables'Range loop
      if Active_Rows(I) then
         Result_Row := Result_Row + 1;
         Result.Basis_Variables(Result_Row) := The_Program.Basis_Variables(I);
      end if;
   end loop;
   if Result_Row /= Result.Last_Row then
      raise Internal_Error;
   end if;
   for J in The_Program.Simplex_Table'Range(2) loop
      if Active_Columns(J) then
         Result_Column := Result_Column + 1;
         if J >= The_Program.First_Column_1 then
            Result.Nonbasis_Variables(Result_Column) := The_Program.Nonbasis_Variables(J);
         end if;
         Result_Row := Result.First_Row - 1;
         for I in The_Program.Simplex_Table'Range(1) loop
            if Active_Rows(I) then
               Result_Row := Result_Row + 1;
               Result.Simplex_Table(Result_Row, Result_Column) :=
                  The_Program.Simplex_Table(I, J);
            end if;
         end loop;
         if Result_Row /= Result.Last_Row then
            raise Internal_Error;
         end if;
      end if;
   end loop;
   if Result_Column /= Result.Last_Column then
      raise Internal_Error;
   end if;
   Result.Bound_Variables_Kind := The_Program.Bound_Variables_Kind;
   Result.Last_Variable := The_Program.Last_Variable;
   Result.State := The_Program.State;
   Result.Pivots_Phase_1 := The_Program.Pivots_Phase_1;
   Result.Pivots_Phase_2 := The_Program.Pivots_Phase_2;
   Result.Pivots_Dual := The_Program.Pivots_Dual;
   Destroy(The_Program);
   The_Program := Result;
end Compress;
