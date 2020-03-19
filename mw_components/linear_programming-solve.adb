-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  5-SEP-1990 by Mats Weber.


separate (Linear_Programming)

procedure Solve (The_Program : in out Program) is

   type Boolean_Vector is array (Integer range <>) of Boolean;


   procedure Compress (The_Program        : in out Program;
                       New_First_Row      : in Integer;
                       Active_Rows        : in Boolean_Vector;
                       Last_Active_Row    : in Natural;
                       Active_Columns     : in Boolean_Vector;
                       Last_Active_Column : in Natural) is separate;


   procedure Swap is new Exchange(Variable_Index);

   procedure Pivot (On_Program     : in out Program_Record;
                    Active_Columns : in Boolean_Vector;
                    P              : in Positive;
                    Q              : in Positive) is

      A      : Simplex_Matrix renames On_Program.Simplex_Table;
      Pivot  : constant Real := A(P, Q);

   begin
      Swap(On_Program.Basis_Variables(P), On_Program.Nonbasis_Variables(Q));
      -- elements in pivot line
      for J in A'Range(2) loop
         if Active_Columns(J) and J /= Q then
            A(P, J) := A(P, J) / Pivot;
         end if;
      end loop;
      -- elements not in pivot line or column
      for J in A'Range(2) loop
         if Active_Columns(J) and J /= Q then
            for I in A'Range(1) loop
               if I /= P then
                  A(I, J) := A(I, J) - A(P, J) * A(I, Q);
               end if;
            end loop;
         end if;
      end loop;
      -- elements in pivot column
      if Active_Columns(Q) then
         for I in A'Range(1) loop
            if I /= P then
               A(I, Q) := - A(I, Q) / Pivot;
            end if;
         end loop;
         A(P, Q) := 1.0 / Pivot;
      end if;
   end Pivot;

   pragma Inline(Pivot);


   generic
      Last_Variable : in Variable_Index;
   function Check_Artificial (The_Variable : Variable_Index) return Boolean;

   function Check_Artificial (The_Variable : Variable_Index) return Boolean is
   begin
      return The_Variable > Last_Variable;
   end;

   pragma Inline(Check_Artificial);


   procedure Primal_Phase_1 (On_Program : in out Program) is separate;

   procedure Primal_Phase_2 (On_Program : in out Program) is separate;

   procedure Dual_Phase (On_Program : in out Program) is separate;


begin
   Check_Program(The_Program);
   case The_Program.State is
      when Do_Two_Phase =>
         Primal_Phase_1(On_Program => The_Program);
         Primal_Phase_2(On_Program => The_Program);
      when Do_Primal =>
         Primal_Phase_2(On_Program => The_Program);
      when Do_Dual =>
         Dual_Phase(On_Program => The_Program);
      when Do_Dual_And_Two_Phase =>
         Dual_Phase(On_Program => The_Program);
         Primal_Phase_1(On_Program => The_Program);
         Primal_Phase_2(On_Program => The_Program);
      when Solved | Not_Solvable | Simplex_Unbounded =>
         null;
   end case;
end Solve;
