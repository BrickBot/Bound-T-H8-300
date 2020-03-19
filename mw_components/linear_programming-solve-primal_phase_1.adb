-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  5-SEP-1990 by Mats Weber.


separate (Linear_Programming.Solve)

procedure Primal_Phase_1 (On_Program : in out Program) is
begin
   case On_Program.State is
      when Do_Two_Phase =>
         null;
      when Do_Primal | Do_Dual | Do_Dual_And_Two_Phase =>
         raise Program_In_Wrong_State;
      when Solved | Not_Solvable | Simplex_Unbounded =>
         return;
   end case;
   declare

      The_Program : Program_Record renames On_Program.all;

      subtype Row_Index   is Integer range The_Program.Simplex_Table'Range(1);
      subtype Row_Index_0 is Row_Index range The_Program.First_Row_1 - 1..Row_Index'Last;
      subtype Row_Index_1 is Row_Index_0 range The_Program.First_Row_1..Row_Index'Last;

      subtype Column_Index   is Natural range The_Program.Simplex_Table'Range(2);
      subtype Column_Index_1 is Column_Index range The_Program.First_Column_1..Column_Index'Last;

      W_Row                 : constant Row_Index := Row_Index'First;
      Z_Row                 : constant Row_Index := Row_Index_0'First;
      B_Column              : constant Column_Index := Column_Index'First;

      A                     : Simplex_Matrix renames The_Program.Simplex_Table;

      Active_Columns        : Boolean_Vector(Column_Index) := (others => True);
         -- columns that contain artificial variables are removed from this set as they
         -- are taken out of the basis during the first phase and are not needed anymore

      Last_Active_Column    : Column_Index := Column_Index'Last;

      P                     : Row_Index_1;
      Q                     : Column_Index;

      function Is_Artificial is new Check_Artificial(The_Program.Last_Variable);


      procedure Select_Pivot_Column (Column : out Column_Index) is
         -- returns Column_Index'First if no pivot column
         -- can be found

         Best_Cost  : Real := Epsilon;
         Q          : Column_Index := Column_Index'First;

      begin
         for J in Column_Index_1 loop
            if Active_Columns(J) and then A(W_Row, J) > Best_Cost then
               Q := J;
               Best_Cost := A(W_Row, J);
            end if;
         end loop;
         Column := Q;
      end Select_Pivot_Column;

      procedure Select_Pivot_Row (Column : in Column_Index_1;
                                  Row    : out Row_Index_0) is
         -- returns Row_Index_0'First if no pivot row
         -- can be found

         Quotient       : Real;
         Best_Quotient  : Real := Real'Last;
         P              : Row_Index_0 := Row_Index_0'First;

      begin
         for I in Row_Index_1 loop
            if A(I, Column) > Epsilon then
               Quotient := A(I, B_Column) / A(I, Column);
               -- try to eliminate artificial variables first
               if P >= Row_Index_1'First and then
                  not Is_Artificial(The_Program.Basis_Variables(P)) and then
                  Is_Artificial(The_Program.Basis_Variables(I))
               then
                  if Quotient * (1.0 - Epsilon) < Best_Quotient then
                     P := I;
                     Best_Quotient := Quotient * (1.0 - Epsilon);
                  end if;
               else
                  if Quotient < Best_Quotient then
                     P := I;
                     Best_Quotient := Quotient;
                  end if;
               end if;
            end if;
         end loop;
         Row := P;
      end Select_Pivot_Row;

      pragma Inline(Select_Pivot_Column, Select_Pivot_Row);


   begin
      loop
         Select_Pivot_Column(Column => Q);
         exit when Q < Column_Index_1'First;  -- no pivot column has been found
         Select_Pivot_Row(Column => Q, Row => P);
         if Is_Artificial(The_Program.Basis_Variables(P)) then
            Active_Columns(Q) := False;
            Last_Active_Column := Last_Active_Column - 1;
         end if;
         The_Program.Pivots_Phase_1 := The_Program.Pivots_Phase_1 + 1;
         Pivot(On_Program     => The_Program,
               Active_Columns => Active_Columns,
               P => P,
               Q => Q);
      end loop;
      --
      declare

         Active_Rows      : Boolean_Vector(Row_Index) := (others => True);
         Last_Active_Row  : Row_Index := Row_Index'Last;
         New_First_Row    : Row_Index range W_Row..Z_Row;

      begin
         if A(W_Row, B_Column) > Epsilon then
            The_Program.State := Not_Solvable;
            New_First_Row := W_Row;
         else
            Active_Rows(W_Row) := False;
            -- remove remaining artificial variables
            for I in Row_Index_1 loop
               if Is_Artificial(The_Program.Basis_Variables(I)) then
                  if abs A(I, B_Column) < Epsilon then
                     Active_Rows(I) := False;
                     Last_Active_Row := Last_Active_Row - 1;
                  else
                     raise Internal_Error;
                  end if;
               end if;
            end loop;
            The_Program.State := Do_Primal;
            New_First_Row := Z_Row;
         end if;
         Compress(The_Program        => On_Program,
                  New_First_Row      => New_First_Row,
                  Active_Columns     => Active_Columns,
                  Last_Active_Column => Last_Active_Column,
                  Active_Rows        => Active_Rows,
                  Last_Active_Row    => Last_Active_Row);
      end;
   end;
end Primal_Phase_1;
