-- PROGRAM TO CHECK AT RANDOM IF THERE IS A WORD OF WEIGHT t IN BCH(p,n,t)
   -----------------------------------------------------------------------

-- Creation : 18-AUG-1989 by Mats Weber.


-- Format of input file :
--
-- <p> <n> <t>


with Text_IO,
     Integer_Text_IO,
     Number_Images,
     ZpZ_Base_Type,
     GF_Base_Type,
     Check_BCH_Bound_Common,
     Random,
     CPU;

use Text_IO,
    ZpZ_Base_Type,
    GF_Base_Type;

procedure Check_BCH_Bound_Random is
--------------------------------

   function Next_Integer return Integer is

      N : Integer;

   begin
      Integer_Text_IO.Get(N);
      return N;
   end;

begin
   Random.Randomize;
   declare

      P : constant ZpZ_Positive := ZpZ_Positive(Next_Integer);
      N : constant Positive     := Next_Integer;
      T : constant Positive     := Next_Integer;

      package BCH_Bound_Utilities is new Check_BCH_Bound_Common(P, N, T);

      use BCH_Bound_Utilities,
          BCH_Bound_Utilities.GF_P_N,
          BCH_Bound_Utilities.GF_P_N.Base_ZpZ_Field,
          BCH_Bound_Utilities.GF_P_N.Base_ZpZ_Field.ZpZ_Polynomials,
          BCH_Bound_Utilities.BCH_Code_P_N_T,
          BCH_Bound_Utilities.GF_P_N_Matrices;


      function Uniform_Positive_Word_Index is new Random.Enumeration_Uniform(Positive_Word_Index);


      function Image is new Number_Images.Integer_Image(Integer);
      function Image is new Number_Images.Integer_Image(GF_Positive);
      function Image is new Number_Images.Integer_Image(ZpZ_Positive);
      function Image is new Number_Images.Fixed_Image(Duration, Default_Aft => 3);
      function Image is new Number_Images.Float_Image(Float, Default_Aft => 3);


      Lambda_Mat           : Matrix(Horla, Horla);           -- Lambda_Mat(I, J) = Lambda ** (I * E(J))
      A                    : Horla_Vector;

      E                    : Word_Indices;


      Counter              : Natural := 0;

      Info_Interval        : constant Duration := 20 * 60.0;   -- 20 CPU minutes
      Min_Info_Iterations  : constant := 100;
      Put_Info_Messages    : Boolean := True;

      CPU_Clock            : CPU.CPU_Counter;

      BCH_Code_Name        : constant String := "BCH(" & Image(P) & "," & Image(N) & "," & Image(T) & ")";


   begin
      Put_Line("Irreducible polynomial used to represent GF(" & Image(Q) & ") :");
      Put(GF_Polynomial);
      New_Line(2);
      Put_Line("Polynomial representation of the generator of GF(" & Image(Q) &
               ") used in " & BCH_Code_Name & " :");
      Put(To_Polynomial(Lambda));
      New_Line(2);
      Put_Line("Generator polynomial used for " & BCH_Code_Name & " :");
      Put(Generator_Polynomial);
      New_Line(2);
      Put("CPU time for startup (elaboration of packages) : " & Image(CPU.CPU_Time) & " seconds");
      New_Line(2);
      Put("Searching at random for a word of weight " & Image(T) & " in " & BCH_Code_Name);
      New_Line(2);
      CPU.Start_Counter(CPU_Clock);
      --
      Main_Loop:
         loop
            for I in Horla loop
               loop
                  declare

                     OK : Boolean := True;

                  begin
                     E(I) := Uniform_Positive_Word_Index;
                     for J in E'First..I - 1 loop
                        if E(J) = E(I) then
                           OK := False;
                           exit;
                        end if;
                     end loop;
                     exit when OK;
                  end;
               end loop;
               for H in Horla loop
                  Lambda_Mat(H, I) := Lambda_K(H) ** GF_Integer(E(I));
               end loop;
            end loop;
            --
            Counter := Counter + 1;
            A := Linear_System_Solution(Lambda_Mat, Minus_Ones);
            if All_In_ZpZ(A) then
               declare

                  Codeword : constant Word := To_Word(Indices => E, Values => A);

               begin
                  New_Line;
                  Put_Line(BCH_Code_Name & " contains the following word of weight " & Image(T) & " :");
                  Put(Polynomial(Codeword));
                  New_Line(2);
                  if In_Code(Codeword) then
                     Put_Line("Result is consistent with function In_Code");
                  else
                     Put_Line("Result is not consistent with function In_Code");
                  end if;
               end;
               exit Main_Loop;
            end if;
            --
            if Counter mod Min_Info_Iterations = 0 and then CPU.CPU_Time(CPU_Clock) >= Info_Interval then
               if Put_Info_Messages then
                  Put_Line(Image(Counter, Width => Natural'Width) & " iterations, " &
                           Image(Float(Counter) / Float(CPU.CPU_Time(CPU_Clock)), Fore => 5) &
                           " linear systems/second");
                  Put_Info_Messages := False;
               end if;
               Counter := 0;
               CPU.Start_Counter(CPU_Clock);
            end if;
         end loop Main_Loop;
   end;
end Check_BCH_Bound_Random;
