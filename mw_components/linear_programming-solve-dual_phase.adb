-- PACKAGE FOR SOLVING LINEAR PROGRAMS
   -----------------------------------

-- Creation :  5-SEP-1990 by Mats Weber.


separate (Linear_Programming.Solve)

procedure Dual_Phase (On_Program : in out Program) is
begin
   case On_Program.State is
      when Do_Dual | Do_Dual_And_Two_Phase =>
         null;
      when Do_Two_Phase | Do_Primal =>
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

      Z_Row               : constant Row_Index := Row_Index_0'First;
      B_Column            : constant Column_Index := Column_Index'First;

      A                   : Simplex_Matrix renames The_Program.Simplex_Table;

      Active_Columns      : Boolean_Vector(Column_Index) := (others => True);
         -- columns that contain artificial variables are removed from this set as they
         -- are taken out of the basis during the first phase and are not needed anymore

      Last_Active_Column  : Column_Index := Column_Index'Last;

      P                   : Row_Index_0;
      Q                   : Column_Index;

      function Is_Artificial is new Check_Artificial(The_Program.Last_Variable);


      procedure Select_Pivot_Row (Row : out Row_Index_0) is
         -- returns Row_Index_0'First if no pivot row
         -- can be found

         Best_Cost  : Real := -Epsilon;
         P          : Row_Index_0 := Row_Index_0'First;

      begin
         for I in Row_Index_1 loop
            if A(I, B_Column) < Best_Cost then
               P := I;
               Best_Cost := A(I, B_Column);
            end if;
         end loop;
         Row := P;
      end Select_Pivot_Row;

      procedure Select_Pivot_Column (Row    : in Row_Index_1;
                                     Column : out Column_Index) is
         -- returns Column_Index'First if no pivot column
         -- can be found

         Quotient       : Real;
         Best_Quotient  : Real := Real'First;
         Q              : Column_Index := Column_Index'First;

      begin
         for J in Column_Index_1 loop
            if Active_Columns(J) and then A(Row, J) < -Epsilon then
               Quotient := A(Z_Row, J) / A(Row, J);
               if Quotient > Best_Quotient then
                  Q := J;
                  Best_Quotient := Quotient;
               end if;
            end if;
         end loop;
         Column := Q;
      end Select_Pivot_Column;

      pragma Inline(Select_Pivot_Row, Select_Pivot_Column);


   begin
      loop
         Select_Pivot_Row(Row => P);
         if P < Row_Index_1'First then
            -- no pivot row has been found
            case The_Program.State is
               when Do_Dual =>
                  The_Program.State := Solved;
               when Do_Dual_And_Two_Phase =>
                  The_Program.State := Do_Two_Phase;
               when others =>
                  raise Internal_Error;
            end case;
            exit;
         end if;
         Select_Pivot_Column(Row => P, Column => Q);
         if Q < Column_Index_1'First then
            The_Program.State := Not_Solvable;
            exit;
         end if;
         if Is_Artificial(The_Program.Basis_Variables(P)) then
            Active_Columns(Q) := False;
            Last_Active_Column := Last_Active_Column - 1;
         end if;
         The_Program.Pivots_Dual := The_Program.Pivots_Dual + 1;
         Pivot(On_Program     => The_Program,
               Active_Columns => Active_Columns,
               P => P,
               Q => Q);
      end loop;
      --
      if Last_Active_Column < Column_Index'Last then
         Compress(The_Program        => On_Program,
                  New_First_Row      => Row_Index'First,
                  Active_Columns     => Active_Columns,
                  Last_Active_Column => Last_Active_Column,
                  Active_Rows        => (Row_Index => True),
                  Last_Active_Row    => Row_Index'Last);
      end if;
   end;
end Dual_Phase;
