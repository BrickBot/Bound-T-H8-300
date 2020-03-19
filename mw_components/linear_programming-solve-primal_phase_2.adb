-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  5-SEP-1990 by Mats Weber.


separate (Linear_Programming.Solve)

procedure Primal_Phase_2 (On_Program : in out Program) is
begin
   case On_Program.State is
      when Do_Primal =>
         null;
      when Do_Two_Phase | Do_Dual | Do_Dual_And_Two_Phase =>
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

      Z_Row      : constant Row_Index := Row_Index_0'First;
      B_Column   : constant Column_Index := Column_Index'First;

      A          : Simplex_Matrix renames The_Program.Simplex_Table;

      P          : Row_Index_0;
      Q          : Column_Index;


      procedure Select_Pivot_Column (Column : out Column_Index) is
         -- returns Column_Index'First if no pivot column
         -- can be found

         Best_Cost  : Real := -Epsilon;
         Q          : Column_Index := Column_Index'First;

      begin
         for J in Column_Index_1 loop
            if A(Z_Row, J) < Best_Cost then
               Q := J;
               Best_Cost := A(Z_Row, J);
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
               if Quotient < Best_Quotient then
                  P := I;
                  Best_Quotient := Quotient;
               end if;
            end if;
         end loop;
         Row := P;
      end Select_Pivot_Row;

      pragma Inline(Select_Pivot_Column, Select_Pivot_Row);


   begin
      loop
         Select_Pivot_Column(Column => Q);
         if Q < Column_Index_1'First then
            -- no pivot column has been found
            The_Program.State := Solved;
            exit;
         end if;
         Select_Pivot_Row(Column => Q, Row => P);
         if P < Row_Index_1'First then
            The_Program.State := Simplex_Unbounded;
            exit;
         end if;
         The_Program.Pivots_Phase_2 := The_Program.Pivots_Phase_2 + 1;
         Pivot(On_Program     => The_Program,
               Active_Columns => (Column_Index => True),
               P => P,
               Q => Q);
      end loop;
   end;
end Primal_Phase_2;
