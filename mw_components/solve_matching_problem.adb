-- PROCEDURE THAT SOLVES MATCHING PROBLEMS WITH INTEGER PROGRAMMING
   ----------------------------------------------------------------

-- Creation : 13-APR-1990 by Mats Weber.


with Linear_Programming,
     Random,
     Generic_Elementary_Functions,
     Number_Images,
     User_Interface,
     Text_IO;

use Text_IO;

procedure Solve_Matching_Problem is
--------------------------------

   function Natural_Answer is new User_Interface.Integer_Answer(Natural);

   N_Vertices  : constant Natural := Natural_Answer("Number of vertices : ");
   N_Edges     : constant Natural := N_Vertices * (N_Vertices - 1) / 2;

   subtype Vertex_Index is Positive range 1..N_Vertices;
   subtype Edge_Index   is Positive range 1..N_Edges;

   type Real is digits 6;

   Epsilon : constant Real := 100.0 * Real'Epsilon;

   package Real_Linear_Programming is new Linear_Programming(Real, Epsilon);

   use Real_Linear_Programming;

   package Real_Elementary_Functions is new Generic_Elementary_Functions(Real);

   use Real_Elementary_Functions;

   Inconsistency      : exception;


   Cloud              : constant array (Vertex_Index) of Vector(1..2) :=
                        (others => (others => Real(Random.Uniform)));

   Objective          : Vector(Edge_Index);
   Constraints        : Matrix(Vertex_Index, Edge_Index) := (others => (others => 0.0));
   Bounds             : constant Vector(Vertex_Index) := (others => 1.0);
   Inequalities       : constant Inequality_Vector(Vertex_Index) := (others => '<');

   Variables          : Vector(Edge_Index);
   Value              : Real;

   Best_Value         : Real := Real'First;
   N_Linear_Programs  : Natural := 0;

   Cut                : exception;


   function Variable_Index (I, J : Vertex_Index) return Edge_Index is
   begin
      return N_Vertices * (I - 1) - I * (I - 1) / 2 + J - I;
   end;

   function First_Vertex (K : Edge_Index) return Vertex_Index is
   begin
      for I in reverse Vertex_Index loop
         if Variable_Index(I - 1, I) <= K then
            return I - 1;
         end if;
      end loop;
   end;

   function Second_Vertex (K : Edge_Index) return Vertex_Index is

      I : constant Vertex_Index := First_Vertex(K);

   begin
      return I + 1 + K - Variable_Index(I, I + 1);
   end;

   function Is_Integer (X : Real) return Boolean is
   begin
      return abs (X - Real(Integer(X))) < Epsilon;
   end;

   function Image is new Number_Images.Integer_Image(Natural);
   function Image is new Number_Images.Float_Image(Real);


   procedure Solve_Matching (The_Program : in out Program;
                             Variables   : out Vector;
                             Value       : out Real;
                             Level       : in Natural) is

      The_Variables        : Vector(Variables'Range);
      The_Value            : Real;
      Integer_Solution     : Boolean := True;
      Noninteger_Variable  : Edge_Index;

   begin
      Solve(The_Program);
      The_Variables := Real_Linear_Programming.Variables(The_Program);
      The_Value := Real_Linear_Programming.Value(The_Program);
      N_Linear_Programs := N_Linear_Programs + 1;
      if The_Value < Best_Value then
         raise Cut;
      end if;
      for K in The_Variables'Range loop
         if not Is_Integer(The_Variables(K)) then
            Integer_Solution := False;
            Noninteger_Variable := K;
            exit;
         end if;
      end loop;
      if Integer_Solution then
         if The_Value > Best_Value then
            Best_Value := The_Value;
         end if;
      else
         declare

            New_Constraint   : Vector(Variables'Range) := (others => 0.0);

            Variables_1,
            Variables_2      : Vector(Variables'Range);
            Value_1,
            Value_2          : Real;

            First_Cut,
            Second_Cut       : Boolean := False;

         begin
            New_Constraint(Noninteger_Variable) := 1.0;
            declare

               New_Program : Program;

            begin
               Assign(New_Program, Value => The_Program);
               Add_Constraint(To_Program     => New_Program,
                              New_Constraint => New_Constraint,
                              New_Inequality => '=',
                              New_Bound      => 0.0);
               Solve_Matching(New_Program,
                              Variables => Variables_1,
                              Value     => Value_1,
                              Level     => Level + 1);
            exception
               when Cut =>
                  First_Cut := True;
            end;
            declare

               New_Program : Program;

            begin
               Assign(New_Program, Value => The_Program);
               Add_Constraint(To_Program     => New_Program,
                              New_Constraint => New_Constraint,
                              New_Inequality => '=',
                              New_Bound      => 1.0);
               Solve_Matching(New_Program,
                              Variables => Variables_2,
                              Value     => Value_2,
                              Level     => Level + 1);
            exception
               when Cut =>
                  Second_Cut := True;
            end;
            if First_Cut and Second_Cut then
               raise Cut;
            elsif Second_Cut or (not (First_Cut or Second_Cut) and then Value_1 > Value_2) then
               The_Value := Value_1;
               The_Variables := Variables_1;
            else
               The_Value := Value_2;
               The_Variables := Variables_2;
            end if;
         end;
      end if;
      Variables := The_Variables;
      Value := The_Value;
      Destroy(The_Program);
   exception
      when Cut =>
         Destroy(The_Program);
         raise;
   end Solve_Matching;

begin
   for I in Vertex_Index loop
      for J in I + 1..N_Vertices loop
         Objective(Variable_Index(I, J)) := 2.0 - Sqrt((Cloud(J)(1) - Cloud(I)(1)) ** 2 +
                                                       (Cloud(J)(2) - Cloud(I)(2)) ** 2);
      end loop;
   end loop;
   --
   for I in Vertex_Index loop
      for J in I + 1..N_Vertices loop
         Constraints(I, Variable_Index(I, J)) := 1.0;
      end loop;
      for J in 1..I - 1 loop
         Constraints(I, Variable_Index(J, I)) := 1.0;
      end loop;
   end loop;
   declare

      The_Program : Program;

   begin
      Create_Program(The_Program,
                     Objective,
                     Constraints,
                     Inequalities,
                     Bounds);
      Solve_Matching(The_Program,
                     Variables,
                     Value,
                     Level => 0);
   end;
   --
   declare

      N_Zero,
      N_One,
      N_Nonintegral  : Natural := 0;

   begin
      for K in Edge_Index loop
         if abs (Variables(K) - 0.0) < Epsilon then
            N_Zero := N_Zero + 1;
         elsif abs (Variables(K) - 1.0) < Epsilon then
            N_One := N_One + 1;
         else
            N_Nonintegral := N_Nonintegral + 1;
         end if;
      end loop;
      if N_One /= N_Vertices / 2 or N_Nonintegral /= 0 or N_Zero + N_One /= N_Edges then
         raise Inconsistency;
      end if;
   end;
   Put_Line("L / Sqrt(N) = " & Image((Real(N_Vertices) - Value) / Sqrt(Real(N_Vertices))));
   Put_Line("Number of linear programs solved : " & Image(N_Linear_Programs));
   -- output in Mathematica form
   New_Line;
   Put_Line("{");
   for I in Cloud'Range loop
      Put("   Point[{" & Image(Cloud(I)(1)) & ", " & Image(Cloud(I)(2)) & "}]");
      if I < Cloud'Last then
         Put(',');
      end if;
      New_Line;
   end loop;
   Put_Line("}");
   New_Line;
   Put_Line("{");
   declare

      N : Natural range 0..N_Vertices / 2 := 0;

   begin
      for K in Variables'Range loop
         if Variables(K) > 0.5 then
            N := N + 1;
            Put("   Line[{{" & Image(Cloud(First_Vertex(K))(1)) & ", " &
                               Image(Cloud(First_Vertex(K))(2)) & "}, {" &
                               Image(Cloud(Second_Vertex(K))(1)) & ", " &
                               Image(Cloud(Second_Vertex(K))(2)) & "}}]");
            if N < N_Vertices / 2 then
               Put(',');
            end if;
            New_Line;
         end if;
      end loop;
   end;
   Put_Line("}");
end Solve_Matching_Problem;
