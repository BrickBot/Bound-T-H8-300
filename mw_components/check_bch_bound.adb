-- PROGRAM TO CHECK IF THERE IS A WORD OF WEIGHT t IN BCH(p,n,t)
   -------------------------------------------------------------

-- Revision : 14-OCT-1988 by Mats Weber, removed the save interval from the input file and added automatic
--                                       save every SAVE_INTERVAL CPU seconds.

-- Creation : 21-FEB-1987 by Mats Weber.


-- Format of input file :
--
-- <p> <n> <t>


with Text_IO,
     Integer_Text_IO,
     Min_Max_Functions,
     Number_Images,
     ZpZ_Base_Type,
     GF_Base_Type,
     Check_BCH_Bound_Common,
     CPU;

use Text_IO,
    ZpZ_Base_Type,
    GF_Base_Type;

procedure Check_BCH_Bound is
-------------------------

   function Next_Integer return Integer is

      N : Integer;

   begin
      Integer_Text_IO.Get(N);
      return N;
   end;

begin
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


      function Image is new Number_Images.Integer_Image(Integer);
      function Image is new Number_Images.Integer_Image(Horla);
      function Image is new Number_Images.Integer_Image(GF_Positive);
      function Image is new Number_Images.Integer_Image(ZpZ_Positive);
      function Image is new Number_Images.Fixed_Image(Duration, Default_Aft => 3);
      function Image is new Number_Images.Float_Image(Float, Default_Aft => 3);


      Lambda_Mat           : Matrix(Horla, Horla);           -- Lambda_Mat(I, J) = Lambda ** (I * E(J))
      A                    : Horla_Vector;

      Saved                : array (Horla) of Boolean := (others => False);
      E, Saved_E           : Word_Indices;


      Counter              : Natural := 0;

      Save_Interval        : constant Duration := 20 * 60.0;   -- 20 CPU minutes
      Min_Save_Iterations  : constant := 100;
      Put_Save_Messages    : Boolean := True;

      CPU_Clock            : CPU.CPU_Counter;

      BCH_Code_Name        : constant String := "BCH(" & Image(P) & "," & Image(N) & "," & Image(T) & ")";
      Save_File_Name       : constant String := "BCH_" & Image(P) & "_" & Image(N) & "_" & Image(T) & ".DAT";


      package Word_Index_Text_IO is new Text_IO.Integer_IO(Positive_Word_Index);


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
      Put("Searching for a word of weight " & Image(T) & " in " & BCH_Code_Name);
      New_Line(2);
      --
      Restore :
         declare

            Save_File : Text_IO.File_Type;

            procedure Restore (File_Name : in String) is
            begin
               Open(Save_File, Name => File_Name, Mode => In_File);
               for I in Saved_E'Range loop
                  Word_Index_Text_IO.Get(Save_File, Saved_E(I));
                  Skip_Line(Save_File);
               end loop;
               Close(Save_File);
               Saved := (others => True);
            end Restore;

         begin
            begin
               Restore(File_Name => Save_File_Name);
            exception
               when End_Error | Data_Error =>
                  Delete(Save_File);  -- delete the invalid save file
                  begin
                     Put_Line("Restoring from previous version of the save file");
                     Restore(File_Name => Save_File_Name);  -- try restoring from the previous version
                  exception
                     when End_Error | Data_Error | Name_Error =>
                        if Is_Open(Save_File) then
                           Close(Save_File);
                        end if;
                        Put_Line("Unable to restore saved position");
                        return;
                  end;
            end;
            --
            Put_Line("Restored saved position :");
            for I in Saved_E'Range loop
               Put("  E" & Image(I) & " = ");
               Word_Index_Text_IO.Put(Saved_E(I), Width => 0);
            end loop;
            New_Line(2);
         exception
            when Name_Error =>
               null;
         end Restore;
      CPU.Start_Counter(CPU_Clock);
      --
      declare

         Found_Word_Of_Weight_T : exception;

         function Max is new Min_Max_Functions.Maximum(Integer);


         procedure Set_E (I : in Horla) is

            First : Positive_Word_Index;

         begin
            if Saved(I) then
               First := Saved_E(I);
               Saved(I) := False;
            elsif I = 1 then
               First := 1;
            elsif I = Horla'Last then
               First := Max(Deg_G, E(I - 1) + 1);
            else
               First := E(I - 1) + 1;
            end if;
            for E_I in First..Word_Index'Last - Integer(Horla'Last - I) loop
               E(I) := E_I;
               for H in Horla loop
                  Lambda_Mat(H, I) := Lambda_K(H) ** GF_Integer(E_I);
               end loop;
               if I = Horla'Last then
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
                     raise Found_Word_Of_Weight_T;
                  end if;
                  --
                  if Counter mod Min_Save_Iterations = 0 and then CPU.CPU_Time(CPU_Clock) >= Save_Interval then
                     Save :
                        declare

                           Save_File : Text_IO.File_Type;

                        begin
                           Create(Save_File, Name => Save_File_Name);
                           for I in E'Range loop
                              Word_Index_Text_IO.Put(Save_File, E(I));
                              New_Line(Save_File);
                           end loop;
                           Close(Save_File);
                        end Save;
                     if Put_Save_Messages then
                        Put_Line(Image(Counter, Width => Natural'Width) & " iterations, " &
                                 Image(Float(Counter) / Float(CPU.CPU_Time(CPU_Clock)), Fore => 5) &
                                 " linear systems/second");
                        Put_Save_Messages := False;
                     end if;
                     Counter := 0;
                     CPU.Start_Counter(CPU_Clock);
                  end if;
               else
                  Set_E(I + 1);
               end if;
            end loop;
         end Set_E;

      begin
         Set_E(1);
         New_Line;
         Put_Line(BCH_Code_Name & " contains no word of weight " & Image(T));
      exception
         when Found_Word_Of_Weight_T => null;
      end;
   end;
end Check_BCH_Bound;
