with Permutations,
     Random_Numbers,
     The_Random_Generator,
     Random_Numeric_Types,
     Tables,
     Number_Images,
     Text_IO,
     User_Interface;

procedure Test_Permutations is
---------------------------

   Test_Error : exception;

   function Natural_Answer is new User_Interface.Integer_Answer(Natural);

   type Element is new Integer range 1..Natural_Answer("Number of elements : ");

   package Element_Permutations is new Permutations(Discrete => Element,
                                                    Count    => Natural);

   use Element_Permutations;


   package Random is
      new Random_Numbers(Int                 => Integer,
                         Real                => Random_Numeric_Types.Random_Real,
                         Generator_Real      => The_Random_Generator.Random_Real,
                         Generator_Uniform   => The_Random_Generator.Uniform,
                         Reset_Generator     => The_Random_Generator.Reset,
                         Randomize_Generator => The_Random_Generator.Randomize);

   function Integer_Uniform (First, Last : Integer) return Integer
      renames Random.Uniform;

   function Uniform is new Random_Permutation(Integer_Uniform);


   function Image is new Number_Images.Integer_Image(Natural);


   Number_Of_Permutations : Natural := 0;


   procedure Put (C : in Cycle) is

      package Element_Text_IO is new Text_IO.Integer_IO(Element);

   begin
      Text_IO.Put('(');
      for I in C.Contents'Range loop
         Element_Text_IO.Put(C.Contents(I), Width => 0);
         if I < C.Contents'Last then
            Text_IO.Put(' ');
         end if;
      end loop;
      Text_IO.Put(')');
   end Put;

   procedure Put (P : in Permutation) is

      P_In_Cycles : constant Cycle_List := Decomposition(P);

   begin
      for I in P_In_Cycles'Range loop
         Put(P_In_Cycles(I));
      end loop;
   end Put;


   procedure Check_Permutation (P : in Permutation) is

      P_In_Cycles          : constant Cycle_List         := Decomposition(P);
      P_In_Transpositions  : constant Transposition_List := Decomposition(P);
      Q                    : Permutation := Identity;

   begin
      Number_Of_Permutations := Number_Of_Permutations + 1;
      if P * Inverse(P) /= Identity then
         raise Test_Error;
      end if;
      for I in P_In_Cycles'Range loop
         Q := P_In_Cycles(I) * Q;
      end loop;
      if Q /= P then
         raise Test_Error;
      end if;
      Q := Identity;
      for I in P_In_Cycles'Range loop
         Q := Q * P_In_Cycles(I);
      end loop;
      if Q /= P then
         raise Test_Error;
      end if;
      Q := Identity;
      for I in reverse P_In_Transpositions'Range loop
         Q := Q * P_In_Transpositions(I);
      end loop;
      if Q /= P then
         raise Test_Error;
      end if;
   end Check_Permutation;

   procedure Check_All_Permutations is new Enumeration(Check_Permutation);


   procedure Check_Random_Permutations is
   begin
      for I in 1..Natural_Answer("Number of random permutations to generate : ") loop
         declare

            P : constant Permutation := Uniform;

            P_In_Cycles         : constant Cycle_List := Decomposition(P);
            P_In_Transpositions : constant Transposition_List := Decomposition(P);

         begin
            for J in reverse P_In_Cycles'Range loop
               Put(P_In_Cycles(J));
            end loop;
            Text_IO.Put(" = ");
            for J in reverse P_In_Transpositions'Range loop
               Put(P_In_Transpositions(J));
            end loop;
            Text_IO.New_Line;
         end;
      end loop;
   end Check_Random_Permutations;


   procedure Check_Distribution_Uniformity is

      package Permutation_To_Natural_Tables is new Tables(Key_Type  => Permutation,
                                                          Item_Type => Natural,
                                                          Count     => Natural);

      use Permutation_To_Natural_Tables;

      Occurences : Permutation_To_Natural_Tables.Table;


      procedure Increment (Count : in out Natural) is
      begin
         Count := Count + 1;
      end;

      procedure Increment_Count is
         new Permutation_To_Natural_Tables.Update(Modify => Increment);


      procedure Put (P : in Permutation; Count : in Natural) is
      begin
         Text_IO.Put(Image(Count, Width => 5) & "  ");
         Put(P);
         Text_IO.New_Line;
      end Put;

      procedure Put_All is new Permutation_To_Natural_Tables.Traversal(Action => Put);

   begin
      for I in 1..Natural_Answer("   Number of random permutations to generate : ") loop
         declare

            P : constant Permutation := Uniform;

         begin
            Increment_Count(Key => P, Within => Occurences);
         exception
            when Permutation_To_Natural_Tables.Nonexistent_Key =>
               Insert(Key => P, Item => 1, Into => Occurences);
         end;
      end loop;
      Put_All(Occurences);
      Text_IO.Put_Line("Number of different permutations seen : " & Image(Card(Occurences)));
      Destroy(Occurences);
   end Check_Distribution_Uniformity;

begin
   if User_Interface.Yes_No_Answer("Check all possible permutations ? ") then
      Check_All_Permutations;
      Text_IO.Put_Line("Number of permutations on " & Image(Natural(Element'Last)) &
                       " elements : " & Image(Number_Of_Permutations));
   end if;
   Check_Random_Permutations;
   if User_Interface.Yes_No_Answer("Check uniformity of Random_Permutation ? ") then
      Check_Distribution_Uniformity;
   end if;
end Test_Permutations;
