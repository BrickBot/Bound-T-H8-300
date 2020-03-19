-- Filename        : test_linear_programming.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : Fri Oct 24 12:54:46 1997
-- Last Modified By: Mats Weber
-- Last Modified On: Fri Oct 24 12:56:22 1997
-- Update Count    : 3


with Linear_Programming,
     Random,
     Random_Numeric_Types,
     Ada.Numerics.Generic_Elementary_Functions,
     Number_Images,
     CPU,
     Text_IO,
     User_Interface;

use Text_IO;

procedure Test_Linear_Programming is
---------------------------------

   type Real is digits 15;

   Test_Error : exception;

   package Real_Linear_Programming is new Linear_Programming(Real,
                                                             Epsilon => 100.0 * Real'Epsilon);

   use Real_Linear_Programming;

   function Natural_Answer is new User_Interface.Integer_Answer(Natural);

   function Image is new Number_Images.Integer_Image(Integer);
   function Image is new Number_Images.Float_Image(Real, Default_Aft => 3, Default_Fore => 2);
   function Image is new Number_Images.Fixed_Image(Duration);


   procedure Put_System (Objective    : in Vector;
                         Constraints  : in Matrix;
                         Inequalities : in Inequality_Vector;
                         Bounds       : in Vector;
                         Indentation  : in Natural := 3) is
   begin
      Put((1..Indentation => ' ') & "max z = ");
      for J in Objective'Range loop
         Put(Image(Objective(J)) & " x" & Image(J));
         if J < Objective'Last then
            Put(" + ");
         end if;
      end loop;
      New_Line;
      for I in Inequalities'Range loop
         Put((1..Indentation => ' '));
         for J in Constraints'Range(2) loop
            Put(Image(Constraints(I, J)) & " x" & Image(J));
            if J < Constraints'Last(2) then
               Put(" + ");
            end if;
         end loop;
         Put(' ' & Inequality'Image(Inequalities(I))(2) & ' ' & Image(Bounds(I)));
         New_Line;
      end loop;
   end Put_System;


   procedure Put_Solution (Variables    : in Vector;
                           Value        : in Real;
                           Indentation  : in Natural := 3) is
   begin
      Put((1..Indentation => ' '));
      for J in Variables'Range loop
         Put('x' & Image(J) & " = " & Image(Variables(J)));
         if J < Variables'Last then
            Put(", ");
         end if;
      end loop;
      New_Line;
      Put_Line((1..Indentation => ' ') & "z = " & Image(Value));
   end Put_Solution;


   procedure Solve_And_Put (Objective    : in Vector;
                            Constraints  : in Matrix;
                            Inequalities : in Inequality_Vector;
                            Bounds       : in Vector;
                            Variables    : out Vector;
                            Value        : out Real) is

      Pivots_1, Pivots_2  : Natural;
      The_Variables       : Vector(Variables'Range);
      The_Value           : Real;

   begin
      New_Line;
      Put_System(Objective,
                 Constraints,
                 Inequalities,
                 Bounds);
      New_Line;
      Solve_Program(Objective,
                    Constraints,
                    Inequalities,
                    Bounds,
                    The_Variables,
                    The_Value,
                    Phase_1_Pivots => Pivots_1,
                    Phase_2_Pivots => Pivots_2);
      Variables := The_Variables;
      Value := The_Value;
      Put_Solution(The_Variables,
                   The_Value);
      Put_Line("   first phase pivots : " & Image(Pivots_1) & ", second phase pivots " & Image(Pivots_2));
   exception
      when No_Solution =>
         Put_Line("   System has no solution");
         raise;
      when Unbounded_Simplex =>
         Put_Line("   Simplex is unbounded");
         raise;
      when Bound_Mismatch =>
         Put_Line("   Bound mismatch");
         raise;
   end Solve_And_Put;

   procedure Solve_And_Put (Objective    : in Vector;
                            Constraints  : in Matrix;
                            Inequalities : in Inequality_Vector;
                            Bounds       : in Vector) is

      Variables  : Vector(Constraints'Range(2));
      Value      : Real;

   begin
      Solve_And_Put(Objective,
                    Constraints,
                    Inequalities,
                    Bounds,
                    Variables,
                    Value);
   end Solve_And_Put;

begin
   declare

      Objective     : constant Vector(1..8) := (1.0, 1.0, 1.0, others => 0.0);
      Constraints   : constant Matrix(1..5, 1..8) := (( -1.0,  1.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0),
                                                      (  1.0,  4.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0),
                                                      (  2.0,  1.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0),
                                                      (  3.0, -4.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0),
                                                      (  0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  1.0));
      Inequalities  : constant Inequality_Vector(1..5) := (others => '=');
      Bounds        : constant Vector(1..5) := (5.0, 45.0, 27.0, 24.0, 4.0);

      Variables     : Vector(1..8);
      Value         : Real;


      Objective_2     : constant Vector(11..18) := Objective;
      Constraints_2   : constant Matrix(21..25, 11..18) := Constraints;
      Inequalities_2  : constant Inequality_Vector(21..25) := Inequalities;
      Bounds_2        : constant Vector(21..25) := Bounds;

      Variables_2     : Vector(11..18);
      Value_2         : Real;

   begin
      Solve_And_Put(Objective,
                    Constraints,
                    Inequalities,
                    Bounds,
                    Variables,
                    Value);
      Solve_Program(Objective_2,
                    Constraints_2,
                    Inequalities_2,
                    Bounds_2,
                    Variables_2,
                    Value_2);
      if Value_2 /= Value or Variables_2 /= Variables then
         raise Test_Error;
      end if;
   end;
   --
   Put_Line("--");
   begin
      Solve_And_Put(Objective    => (1.0, 1.0, 0.0, 0.0, 0.0),
                    Constraints  => ((2.0, -1.0, -1.0,  0.0, 0.0),
                                     (1.0, -1.0,  0.0, -1.0, 0.0),
                                     (0.0,  1.0,  0.0,  0.0, 1.0)),
                    Inequalities => (1..3 => '='),
                    Bounds       => (-1.0, -1.0, 1.0));
      raise Test_Error;
   exception
      when Unbounded_Simplex => null;
   end;
   --
   Put_Line("--");
   begin
      Solve_And_Put(Objective    => (1.0, 1.0),
                    Constraints  => (1 => (1.0, 1.0)),
                    Inequalities => (1 => '='),
                    Bounds       => (1 => -1.0));
      raise Test_Error;
   exception
      when No_Solution => null;
   end;
   --
   Put_Line("--");
   declare

      Variables  : Vector(1..3);
      Value      : Real;

      Variables_2  : Vector(11..13);
      Value_2      : Real;

      Variables_3  : Vector(21..23);
      Value_3      : Real;

   begin
      Solve_And_Put(Objective    => (2.0, 1.0, 0.0),
                    Constraints  => ((1.0,  2.0, 1.0),
                                     (3.0, -2.0, 3.0)),
                    Inequalities => ('=', '='),
                    Bounds       => (5.0, 2.0),
                    Variables    => Variables,
                    Value        => Value);
      Solve_Program(Objective    => (11 => 2.0, 12 => 1.0, 13 => 0.0),
                    Constraints  => (21 => (11 => 1.0,  12 => 2.0, 13 => 1.0),
                                     22 => (11 => 3.0, 12 => -2.0, 13 => 3.0)),
                    Inequalities => (21 => '=', 22 => '='),
                    Bounds       => (21 => 5.0, 22 => 2.0),
                    Variables    => Variables_2,
                    Value        => Value_2);
      Solve_Program(Objective    => (21 => 2.0, 22 => 1.0, 23 => 0.0),
                    Constraints  => (11 => (21 => 1.0,  22 => 2.0, 23 => 1.0),
                                     12 => (21 => 3.0, 22 => -2.0, 23 => 3.0)),
                    Inequalities => (11 => '=', 12 => '='),
                    Bounds       => (11 => 5.0, 12 => 2.0),
                    Variables    => Variables_3,
                    Value        => Value_3);
      if Value_2 /= Value or Variables_2 /= Variables or
         Value_3 /= Value or Variables_3 /= Variables
      then
         raise Test_Error;
      end if;
   end;
   --
   Put_Line("--");
   declare

      Variables  : Vector(1..2);
      Value      : Real;

   begin
      Solve_And_Put(Objective    => (5.0, 8.0),
                    Constraints  => ((1.0,  1.0),
                                     (1.0, -2.0),
                                     (-1.0, 4.0)),
                    Inequalities => ('<', '<', '<'),
                    Bounds       => (2.0, 0.0, 1.0),
                    Variables    => Variables,
                    Value        => Value);
   end;
   --
   Put_Line("--");
   begin
      Solve_And_Put(Objective    => (5.0, -2.0, -3.0, 7.0),
                    Constraints  => (( 1.0,  1.0,  1.0, -2.0),
                                     ( 2.0,  3.0,  1.0, -1.0),
                                     ( 1.0, -1.0,  1.0, -4.0),
                                     (-2.0,  1.0, -2.0,  7.0)),
                    Inequalities => ('=', '>', '<', '<'),
                    Bounds       => (-1.0, 4.0, 3.0, -2.0));
      raise Test_Error;
   exception
      when No_Solution => null;
   end;
   --
   Put_Line("--");
   declare

      Variables  : Vector(1..5);
      Value      : Real;

      Variables_2  : Vector(31..35);
      Value_2      : Real;

      Variables_3  : Vector(71..75);
      Value_3      : Real;

   begin
      Solve_And_Put(Objective    => (5.0, -2.0, -3.0, 7.0, 3.0),
                    Constraints  => (( 1.0,  1.0,  1.0, -2.0, -1.0),
                                     ( 2.0,  3.0,  1.0, -1.0,  5.0),
                                     ( 1.0, -1.0,  1.0, -4.0,  0.0),
                                     (-2.0,  1.0, -2.0,  7.0,  4.5)),
                    Inequalities => ('=', '>', '<', '<'),
                    Bounds       => (3.0, 4.0, 3.0, -1.0),
                    Variables    => Variables,
                    Value        => Value);
      Solve_Program(Objective    => (31 => 5.0, 32 => -2.0, 33 => -3.0, 34 => 7.0, 35 => 3.0),
                    Constraints  => (71 => (31 =>  1.0, 32 =>  1.0, 33 =>  1.0, 34 => -2.0, 35 => -1.0),
                                     72 => (31 =>  2.0, 32 =>  3.0, 33 =>  1.0, 34 => -1.0, 35 =>  5.0),
                                     73 => (31 =>  1.0, 32 => -1.0, 33 =>  1.0, 34 => -4.0, 35 =>  0.0),
                                     74 => (31 => -2.0, 32 =>  1.0, 33 => -2.0, 34 =>  7.0, 35 =>  4.5)),
                    Inequalities => (71 => '=', 72 => '>', 73 => '<', 74 => '<'),
                    Bounds       => (71 => 3.0, 72 => 4.0, 73 => 3.0, 74 => -1.0),
                    Variables    => Variables_2,
                    Value        => Value_2);
      Solve_Program(Objective    => (71 => 5.0, 72 => -2.0, 73 => -3.0, 74 => 7.0, 75 => 3.0),
                    Constraints  => (31 => (71 =>  1.0, 72 =>  1.0, 73 =>  1.0, 74 => -2.0, 75 => -1.0),
                                     32 => (71 =>  2.0, 72 =>  3.0, 73 =>  1.0, 74 => -1.0, 75 =>  5.0),
                                     33 => (71 =>  1.0, 72 => -1.0, 73 =>  1.0, 74 => -4.0, 75 =>  0.0),
                                     34 => (71 => -2.0, 72 =>  1.0, 73 => -2.0, 74 =>  7.0, 75 =>  4.5)),
                    Inequalities => (31 => '=', 32 => '>', 33 => '<', 34 => '<'),
                    Bounds       => (31 => 3.0, 32 => 4.0, 33 => 3.0, 34 => -1.0),
                    Variables    => Variables_3,
                    Value        => Value_3);
      if Value_2 /= Value or Variables_2 /= Variables or
         Value_3 /= Value or Variables_3 /= Variables
      then
         raise Test_Error;
      end if;
   end;
   --
   Put_Line("--");
<<Again_1>>
   New_Line;
   declare

      package Real_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions(Real);

      use Real_Elementary_Functions,
          Random_Numeric_Types;

      N_Eq : constant Natural := Natural_Answer("Number of ""="" constraints  : ");
      N_Le : constant Natural := Natural_Answer("Number of ""<="" constraints : ");
      N_Ge : constant Natural := Natural_Answer("Number of "">="" constraints : ");
      N    : constant Natural := N_Eq + N_Le + N_Ge;

      M    : constant Natural := Natural_Answer("Number of variables : ");

      Sup_Ge_Radius : constant Real := 1.0 / Sqrt(Real(M));
      Ge_Radius     : constant Real := Real(Random.Uniform(0.0, Random_Real(Sup_Ge_Radius / 2.0))) +
                                       Real(Random.Uniform(0.0, Random_Real(Sup_Ge_Radius / 2.0)));

      A_Solution    : constant Vector(1..M) := (others => Real(Random.Uniform(Random_Real(Ge_Radius),
                                                                              Random_Real(Sup_Ge_Radius))));

      Objective     : Vector(1..M) := (others => 1.0);
      Constraints   : Matrix(1..N, 1..M) := (others => (others => Real(Random.Normal)));
      Inequalities  : constant Inequality_Vector(1..N) := (1..N_Eq => '=') &
                                                          (1..N_Le => '<') &
                                                          (1..N_Ge => '>');
      Bounds        : Vector(1..N) := (1..N_Eq => 0.0) &
                                      (1..N_Le => 1.0) &
                                      (1..N_Ge => Ge_Radius);

      Variables     : Vector(1..M);
      Value         : Real;

      Pivots_1, Pivots_2 : Natural;

   begin
      if User_Interface.Yes_No_Answer("Generate random objective function ? ") then
         Objective := (others => Real(Random.Normal));
      end if;
      for I in 1..N_Eq loop
         for J in Constraints'Range(2) loop
            Bounds(I) := Bounds(I) + Constraints(I, J) * A_Solution(J);
         end loop;
      end loop;
      for I in N_Eq + 1..Constraints'Last(1) loop
         declare

            S : Real := 0.0;

         begin
            for J in Constraints'Range(2) loop
               S := S + Constraints(I, J) ** 2;
            end loop;
            S := Sqrt(S);
            for J in Constraints'Range(2) loop
               Constraints(I, J) := abs(Constraints(I, J)) / S;
            end loop;
         end;
      end loop;
      Put_Line("possible solution (used to generate ""="" constraints) :");
      Put_Solution(A_Solution, Value => 0.0);
      CPU.Start_Counter;
      Solve_Program(Objective,
                    Constraints,
                    Inequalities,
                    Bounds,
                    Variables,
                    Value,
                    Phase_1_Pivots => Pivots_1,
                    Phase_2_Pivots => Pivots_2);
      Put_Line("CPU time used to solve the program : " & Image(CPU.CPU_Time) & " seconds");
      Put_Line("the solution :");
      Put_Solution(Variables, Value);
      Put_Line("   Phase 1 Pivots => " & Image(Pivots_1));
      Put_Line("   Phase 2 Pivots => " & Image(Pivots_2));
      --
      CPU.Start_Counter;
      declare

         Constraints_Taken,
         Equality_Constraints : array (Constraints'Range(1)) of Boolean := (others => False);

         Current_Program : Program;

      begin
         Create_Program(Current_Program,
                        Objective    => Objective,
                        Constraints  => (1 => (1..M => 1.0)),
                        Inequalities => (1 => '<'),
                        Bounds       => (1 => 10_012.0));
         Solve(Current_Program);
         loop
            declare

               New_Row         : Positive range Bounds'Range;
               New_Constraint  : Vector(Constraints'Range(2));

            begin
               if Constraints_Taken = (Constraints_Taken'Range => True) or else Random.Probability(0.25) then
                  Solve(Current_Program);
                  Put_Line("   Phase_1_Pivots => " & Image(Phase_1_Pivots(Current_Program)));
                  Put_Line("   Phase_2_Pivots => " & Image(Phase_2_Pivots(Current_Program)));
                  Put_Line("   Dual_Pivots    => " & Image(Dual_Pivots(Current_Program)));
                  if Constraints_Taken = (Constraints_Taken'Range => True) then
                     Put_Line("CPU time used to solve the program incrementally : " &
                              Image(CPU.CPU_Time) & " seconds");
                     Put_Line("the solution :");
                     Put_Solution(Real_Linear_Programming.Variables(Current_Program),
                                  Real_Linear_Programming.Value(Current_Program));
                     exit;
                  end if;
               end if;
               loop
                  New_Row := Positive(Random.Uniform(First => Random_Integer(Bounds'First),
                                                     Last  => Random_Integer(Bounds'Last)));
                  exit when not Constraints_Taken(New_Row);
               end loop;
               Constraints_Taken(New_Row) := True;
               for J in New_Constraint'Range loop
                  New_Constraint(J) := Constraints(New_Row, J);
               end loop;
               Add_Constraint(To_Program     => Current_Program,
                              New_Constraint => New_Constraint,
                              New_Inequality => Inequalities(New_Row),
                              New_Bound      => Bounds(New_Row));
               Equality_Constraints(New_Row) := True;
               Put_Line("-- " & Inequality'Image(Inequalities(New_Row)) &
                        "-constraint " & Image(New_Row) & " added");
            end;
         end loop;
         --
         if User_Interface.Yes_No_Answer("Change objective ? ") then
            CPU.Start_Counter;
            Change_Objective(On_Program    => Current_Program,
                             New_Objective => (1..M => Real(Random.Normal)));
            Put_Line("CPU time used to change the objective : " &
                     Image(CPU.CPU_Time) & " seconds");
            Solve(Current_Program);
            Put_Line("   Phase_1_Pivots => " & Image(Phase_1_Pivots(Current_Program)));
            Put_Line("   Phase_2_Pivots => " & Image(Phase_2_Pivots(Current_Program)));
            Put_Line("   Dual_Pivots    => " & Image(Dual_Pivots(Current_Program)));
            Put_Line("CPU time used to change the objective and solve : " &
                     Image(CPU.CPU_Time) & " seconds");
            Put_Line("the solution :");
            Put_Solution(Real_Linear_Programming.Variables(Current_Program),
                         Real_Linear_Programming.Value(Current_Program));
            Put_Line("-- Resotring previous objective");
            Change_Objective(On_Program    => Current_Program,
                             New_Objective => Objective);
            Solve(Current_Program);
            Put_Line("   Phase_1_Pivots => " & Image(Phase_1_Pivots(Current_Program)));
            Put_Line("   Phase_2_Pivots => " & Image(Phase_2_Pivots(Current_Program)));
            Put_Line("   Dual_Pivots    => " & Image(Dual_Pivots(Current_Program)));
            Put_Line("the solution :");
            Put_Solution(Real_Linear_Programming.Variables(Current_Program),
                         Real_Linear_Programming.Value(Current_Program));
         end if;
         --
         if User_Interface.Yes_No_Answer("Change bounds ? ") then
            declare

               function Real_Answer is new User_Interface.Float_Answer(Real);

               Lower : constant Real := Real_Answer("Lower bound on bounds difference : ");
               Upper : constant Real := Real_Answer("Upper bound on bounds difference : ");

               Delta_Bounds : Vector(1..N + 1) := 0.0 &
                                                  (1..N => Real(Random.Uniform(First => Random_Real(Lower),
                                                                               Last  => Random_Real(Upper))));
                  -- first component necessary because the program has one additional
                  -- constraint at the beginning

            begin
               for J in Equality_Constraints'Range loop
                  if Equality_Constraints(J) then
                     Delta_Bounds(J) := 0.0;
                  end if;
               end loop;
               CPU.Start_Counter;
               Change_Bounds(On_Program   => Current_Program,
                             Delta_Bounds => Delta_Bounds);
               Put_Line("CPU time used to change the bounds : " &
                        Image(CPU.CPU_Time) & " seconds");
               Solve(Current_Program);
               Put_Line("   Phase_1_Pivots => " & Image(Phase_1_Pivots(Current_Program)));
               Put_Line("   Phase_2_Pivots => " & Image(Phase_2_Pivots(Current_Program)));
               Put_Line("   Dual_Pivots    => " & Image(Dual_Pivots(Current_Program)));
               Put_Line("CPU time used to change the bounds and solve : " &
                        Image(CPU.CPU_Time) & " seconds");
               Put_Line("the solution :");
               Put_Solution(Real_Linear_Programming.Variables(Current_Program),
                            Real_Linear_Programming.Value(Current_Program));
               Put_Line("-- Resotring previous bounds");
               for J in Delta_Bounds'Range loop
                  Delta_Bounds(J) := -Delta_Bounds(J);
               end loop;
               Change_Bounds(On_Program   => Current_Program,
                             Delta_Bounds => Delta_Bounds);
               Solve(Current_Program);
               Put_Line("   Phase_1_Pivots => " & Image(Phase_1_Pivots(Current_Program)));
               Put_Line("   Phase_2_Pivots => " & Image(Phase_2_Pivots(Current_Program)));
               Put_Line("   Dual_Pivots    => " & Image(Dual_Pivots(Current_Program)));
               Put_Line("the solution :");
               Put_Solution(Real_Linear_Programming.Variables(Current_Program),
                            Real_Linear_Programming.Value(Current_Program));
            end;
         end if;
      end;
   end;
   if User_Interface.Yes_No_Answer("More ? (y/n) ") then
      goto Again_1;
   end if;
   --
   Put_Line("--");
<<Again_2>>
   New_Line;
   declare

      File  : Text_IO.File_Type;

      M, N  : Natural := 0;
      Junk  : Real;
      Ch    : Character;

      package Real_Text_IO is new Text_IO.Float_IO(Real);

      use Real_Text_IO;

   begin
      Open(File, Name => User_Interface.String_Answer("Problem file name : "), Mode => In_File);
      -- determine problem size
      loop
         begin
            Get(File, Junk);
            M := M + 1;
         exception
            when Text_IO.Data_Error =>
               Get(File, Ch);
               exit when Ch = '|';
               raise Text_IO.Data_Error;
         end;
      end loop;
      loop
         begin
            Get(File, Junk);
         exception
            when Text_IO.End_Error =>
               exit;
         end;
         for J in 2..M loop
            Get(File, Junk);
         end loop;
         loop
            Get(File, Ch);
            exit when Ch /= ' ';
         end loop;
         Get(File, Junk);
         N := N + 1;
      end loop;
      Reset(File);
      declare

         Objective     : Vector(1..M);
         Constraints   : Matrix(1..N, 1..M);
         Inequalities  : Inequality_Vector(1..N);
         Bounds        : Vector(1..N);

         Variables     : Vector(1..M);
         Value         : Real;

         Pivots_1, Pivots_2 : Natural;

      begin
         for J in 1..M loop
            Get(File, Objective(J));
         end loop;
         loop
            Get(File, Ch);
            exit when Ch = '|';
         end loop;
         for I in 1..N loop
            for J in 1..M loop
               Get(File, Constraints(I, J));
            end loop;
            loop
               Get(File, Ch);
               exit when Ch /= ' ';
            end loop;
            case Ch is
               when '<' =>
                  Inequalities(I) := '<';
               when '=' =>
                  Inequalities(I) := '=';
               when '>' =>
                  Inequalities(I) := '>';
               when others =>
                  raise Text_IO.Data_Error;
            end case;
            Get(File, Bounds(I));
            N := N + 1;
         end loop;
         Close(File);
         begin
            CPU.Start_Counter;
            begin
               Solve_Program(Objective,
                             Constraints,
                             Inequalities,
                             Bounds,
                             Variables,
                             Value,
                             Phase_1_Pivots => Pivots_1,
                             Phase_2_Pivots => Pivots_2);
            exception
               when others =>
                  Put_Line("CPU time used : " & Image(CPU.CPU_Time) & " seconds");
                  raise;
            end;
            Put_Line("CPU time used to solve the program : " & Image(CPU.CPU_Time) & " seconds");
            Put_Solution(Variables, Value);
            Put_Line("   Phase 1 Pivots => " & Image(Pivots_1));
            Put_Line("   Phase 2 Pivots => " & Image(Pivots_2));
         exception
            when No_Solution =>
               Put_Line("--- No Solution ---");
            when Unbounded_Simplex =>
               Put_Line("--- Unbounded Simplex ---");
            when Bound_Mismatch =>
               Put_Line("--- Bound Mismatch ---");
         end;
      end;
      goto Again_2;
   exception
      when Text_IO.Name_Error =>
         if User_Interface.Yes_No_Answer("Not found, try again ? (y/n) ") then
            goto Again_2;
         end if;
      when Text_IO.Data_Error | Text_IO.End_Error =>
         Put_Line("Error in program file");
         Close(File);
         goto Again_2;
      when others =>
         if Is_Open(File) then
            Close(File);
         end if;
         raise;
   end;
end Test_Linear_Programming;
