-- Filename        : test_multiple_key_bags.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : long ago
-- Last Modified By: Mats Weber
-- Last Modified On: Thu May 28 19:12:06 1998
-- Update Count    : 1


with Multiple_Key_Bags,
     Random,
     Random_Numeric_Types,
     CPU,
     Number_Images,
     Text_IO;

use Text_IO;

procedure Test_Multiple_Key_Bags is
--------------------------------

   pragma Time_Slice(0.02);

   type Rec is
      record
         A : Natural;
         B : Integer;
         C : Float;
         D : Float range 0.0 .. 10.0;
         E : Boolean;
         F : Integer;
      end record;

   package Rec_Bags is new Multiple_Key_Bags(Item_Type      => Rec,
                                             Count          => Natural,
                                             Number_Of_Keys => 5);

   function Key_A (X : Rec) return Natural;
   function Key_B (X : Rec) return Integer;
   function Key_C (X : Rec) return Float;
   function Key_D (X : Rec) return Float;
   function Key_E (X : Rec) return Boolean;

   package Rec_Bags_Key_A is new Rec_Bags.Access_By_Key(Key_Type => Natural,
                                                        Key_Of   => Key_A);

   package Rec_Bags_Key_B is new Rec_Bags.Access_By_Key(Key_Type => Integer,
                                                        Key_Of   => Key_B);

   package Rec_Bags_Key_C is new Rec_Bags.Access_By_Key(Key_Type => Float,
                                                        Key_Of   => Key_C);

   package Rec_Bags_Key_D is new Rec_Bags.Access_By_Key(Key_Type => Float,
                                                        Key_Of   => Key_D);

   package Rec_Bags_Key_E is new Rec_Bags.Access_By_Key(Key_Type => Boolean,
                                                        Key_Of   => Key_E);

   use Rec_Bags,
       Rec_Bags_Key_A,
       Rec_Bags_Key_B,
       Rec_Bags_Key_C,
       Rec_Bags_Key_D,
       Rec_Bags_Key_E;

   Some_Recs : constant Rec_Bags.List :=
               ( 1 => (A =>  5, B => -10, C =>  10.3, D => 10.0, E => False, F => -77),
                 2 => (A =>  6, B =>  -9, C =>  23.8, D =>  1.0, E => True , F => -44),
                 3 => (A =>  6, B =>  -8, C =>  78.0, D =>  0.0, E => False, F => -12),
                 4 => (A =>  6, B =>  -7, C =>   0.0, D =>  5.5, E => False, F =>  -1),
                 5 => (A =>  7, B =>  -6, C =>  57.9, D =>  7.7, E => False, F =>   0),
                 6 => (A =>  1, B =>  -5, C =>  12.7, D =>  9.9, E => True , F =>   0),
                 7 => (A =>  1, B =>  -4, C =>  10.3, D =>  2.2, E => True , F =>   0),
                 8 => (A =>  1, B =>  -3, C => -56.8, D => 10.0, E => False, F =>   0),
                 9 => (A =>  1, B =>  -2, C =>   7.7, D =>  1.0, E => True , F =>   0),
                10 => (A =>  0, B =>  -1, C =>   0.1, D =>  4.4, E => False, F =>   0),
                11 => (A =>  0, B =>   0, C =>  -0.1, D =>  2.7, E => True , F => -77),
                12 => (A =>  0, B =>   1, C =>  -1.0, D =>  3.5, E => False, F => -78),
                13 => (A =>  0, B =>   2, C => -17.7, D =>  4.7, E => False, F => -79),
                14 => (A =>  3, B =>   3, C => -20.0, D =>  1.1, E => False, F => -80),
                15 => (A =>  3, B =>   4, C =>   0.1, D =>  1.2, E => True , F =>  81),
                16 => (A =>  3, B =>   5, C =>  10.3, D =>  1.2, E => True , F =>  82),
                17 => (A =>  3, B =>   6, C =>  57.9, D =>  1.2, E => True , F =>  44),
                18 => (A => 12, B =>   7, C => -57.9, D =>  1.0, E => True , F =>  66),
                19 => (A => 12, B =>   8, C => -11.1, D =>  4.7, E => False, F =>   1),
                20 => (A => 13, B =>   9, C => -22.2, D =>  2.2, E => False, F =>   1),
                21 => (A =>  0, B =>  10, C => -33.3, D =>  3.3, E => True , F =>   1),
                22 => (A =>  0, B =>  11, C => -44.4, D =>  6.6, E => True , F =>   4),
                23 => (A =>  7, B =>  12, C =>   0.0, D =>  9.9, E => False, F =>   1),
                24 => (A =>  1, B =>  13, C =>  10.3, D =>  1.1, E => True , F =>  14));

   Bag_1, Bag_2 : Rec_Bags.Bag;

   Test_Error   : exception;


   function Key_A (X : Rec) return Natural is
   begin
      return X.A;
   end;

   function Key_B (X : Rec) return Integer is
   begin
      return X.B;
   end;

   function Key_C (X : Rec) return Float is
   begin
      return X.C;
   end;

   function Key_D (X : Rec) return Float is
   begin
      return X.D;
   end;

   function Key_E (X : Rec) return Boolean is
   begin
      return X.E;
   end;


   function Image is new Number_Images.Fixed_Image(Duration);
   function Image is new Number_Images.Integer_Image(Integer);

begin
   Put_Line("CPU time used for elaboration : " & Image(CPU.CPU_Time) & " seconds");
   for Test_Iteration in 1 .. 3 loop
      for I in Some_Recs'Range loop
         Insert(Item => Some_Recs(I), Into => Bag_1);          -- Inserts SOME_RECS(1 .. 24)
         if not Rec_Bags_Key_A.Member(Key => Some_Recs(I).A, Of_Bag => Bag_1) then
            raise Test_Error;
         end if;
         if not Rec_Bags_Key_B.Member(Key => Some_Recs(I).B, Of_Bag => Bag_1) then
            raise Test_Error;
         end if;
         if not Rec_Bags_Key_C.Member(Key => Some_Recs(I).C, Of_Bag => Bag_1) then
            raise Test_Error;
         end if;
         if not Rec_Bags_Key_D.Member(Key => Some_Recs(I).D, Of_Bag => Bag_1) then
            raise Test_Error;
         end if;
         if not Rec_Bags_Key_E.Member(Key => Some_Recs(I).E, Of_Bag => Bag_1) then
            raise Test_Error;
         end if;
         if Rec_Bags_Key_A.Member(Key => Some_Recs(I).A, Of_Bag => Bag_2) then
            raise Test_Error;
         end if;
         if Rec_Bags_Key_B.Member(Key => Some_Recs(I).B, Of_Bag => Bag_2) then
            raise Test_Error;
         end if;
         if Rec_Bags_Key_C.Member(Key => Some_Recs(I).C, Of_Bag => Bag_2) then
            raise Test_Error;
         end if;
         if Rec_Bags_Key_D.Member(Key => Some_Recs(I).D, Of_Bag => Bag_2) then
            raise Test_Error;
         end if;
         if Rec_Bags_Key_E.Member(Key => Some_Recs(I).E, Of_Bag => Bag_2) then
            raise Test_Error;
         end if;
         if Card(Bag_1) /= I then
            raise Test_Error;
         end if;
      end loop;
      declare

         In_Bag_1 : array (Some_Recs'Range) of Positive :=
                    (10, 11, 12, 13, 21, 22,  6,  7,  8,  9, 24, 14,
                     15, 16, 17,  1,  2,  3,  4,  5, 23, 18, 19, 20);

         Index    : Natural range 0 .. In_Bag_1'Last := 0;

         procedure Check (The_Rec : in Rec) is
         begin
            Index := Index + 1;
            if The_Rec /= Some_Recs(In_Bag_1(Index)) then
               raise Test_Error;
            end if;
         end Check;

         procedure Check_All is new Rec_Bags_Key_A.Traversal(Check);

      begin
         Check_All(Bag_1);
         if Index /= In_Bag_1'Last then
            raise Test_Error;
         end if;
      end;
      declare

         Min_Key_C,
         Max_Key_C   : Rec;

      begin
         Rec_Bags_Key_C.Remove_Min(From => Bag_1, Min => Min_Key_C);  -- Removes SOME_RECS(18)
         Rec_Bags_Key_C.Remove_Max(From => Bag_1, Max => Max_Key_C);  -- Removes SOME_RECS(3)
         if Min_Key_C /= Some_Recs(18) then
            raise Test_Error;
         end if;
         if Max_Key_C /= Some_Recs(3) then
            raise Test_Error;
         end if;
         Insert(Min_Key_C, Into => Bag_1);                            -- Inserts SOME_RECS(18)
         Insert(Max_Key_C, Into => Bag_1);                            -- Inserts SOME_RECS(3)
      end;
      Rec_Bags_Key_B.Remove(Key => -1, From => Bag_1);                -- Removes SOME_RECS(10)
      if Card(Bag_1) /= Some_Recs'Length - 1 then
         raise Test_Error;
      end if;
      begin
         Rec_Bags_Key_A.Remove(Key => 1012, From => Bag_1);           -- Removes nothing
         raise Test_Error;
      exception
         when Rec_Bags.Nonexistent_Key =>
            null;
      end;
      begin
         Rec_Bags_Key_E.Remove(Key => True, From => Bag_2);   -- Removes nothing (BAG_2 is empty)
         raise Test_Error;
      exception
         when Rec_Bags.Nonexistent_Key =>
            null;
      end;
      if Card(Bag_1) /= Some_Recs'Length - 1 then
         raise Test_Error;
      end if;
      if Rec_Bags_Key_A.Search(Key => 1, Within => Bag_1) /= Some_Recs(6) then
         raise Test_Error;
      end if;
      if Rec_Bags_Key_A.Search(Key => 1, Within => Bag_1) /= Some_Recs(6 .. 9) & Some_Recs(24) then
         raise Test_Error;
      end if;
      loop
         begin
            Rec_Bags_Key_A.Remove(Key => 1, From => Bag_1);   -- Removes SOME_RECS(6, 7, 8, 9, 24)
         exception
            when Rec_Bags.Nonexistent_Key =>
               exit;
         end;
      end loop;
      if Card(Bag_1) /= Some_Recs'Length - 1 - 5 then
         raise Test_Error;
      end if;
      if Rec_Bags_Key_A.Search(Key => 12, Within => Bag_1) /= Some_Recs(19) & Some_Recs(18) then
         raise Test_Error;
      end if;
      Assign(Object => Bag_2, Value => Bag_1);
      Destroy(Bag_1);
      if Rec_Bags_Key_A.Search(Key => 3, Within => Bag_2) /= Some_Recs(14 .. 17) then
         raise Test_Error;
      end if;
      if Rec_Bags_Key_A.Search(Key => 555, Within => Bag_2) /= Some_Recs(1 .. 0) then
         raise Test_Error;
      end if;
      if Rec_Bags_Key_A.Min(Of_Bag => Bag_2) /= 0 then
         raise Test_Error;
      end if;
      if Rec_Bags_Key_A.Max(Of_Bag => Bag_2) /= 13 then
         raise Test_Error;
      end if;
      if Rec_Bags_Key_A.Min(Of_Bag => Bag_2) /= Some_Recs(11) then
         raise Test_Error;
      end if;
      if Rec_Bags_Key_B.Min(Of_Bag => Bag_2) /= Some_Recs(1) then
         raise Test_Error;
      end if;
      if Rec_Bags_Key_B.Max(Of_Bag => Bag_2) /= Some_Recs(23) then
         raise Test_Error;
      end if;
      declare

         In_Bag_2 : array (1 .. 18) of Positive :=
                    (1, 2, 3, 4, 5, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);

         Index    : Natural range 0 .. In_Bag_2'Last := 0;

         procedure Check (The_Rec : in Rec) is
         begin
            Index := Index + 1;
            if The_Rec /= Some_Recs(In_Bag_2(Index)) then
               raise Test_Error;
            end if;
         end Check;

         procedure Check_All is new Rec_Bags_Key_B.Traversal(Check);

      begin
         Check_All(Bag_2);
         if Index /= In_Bag_2'Last then
            raise Test_Error;
         end if;
      end;
      Rec_Bags_Key_D.Remove(Key => 10.0, From => Bag_2);                -- Removes SOME_RECS(1)
      declare

         Min_Key_E,
         Max_Key_E   : Rec;

      begin
         Rec_Bags_Key_E.Remove_Min(From => Bag_2, Min => Min_Key_E);    -- Removes SOME_RECS(4)
         Rec_Bags_Key_E.Remove_Max(From => Bag_2, Max => Max_Key_E);    -- Removes SOME_RECS(18)
         if Min_Key_E /= Some_Recs(4) then
            raise Test_Error;
         end if;
         if Max_Key_E /= Some_Recs(18) then
            raise Test_Error;
         end if;
      end;
      Destroy(Bag_2);
   end loop;
   --
   declare

      N : Natural;

      subtype Zero_Four is Natural range 0 .. 4;

      package Natural_Text_IO is new Text_IO.Integer_IO(Natural);
      use Natural_Text_IO;

   begin
      Put("Number of random insertions to perform : ");
      Get(N);
      Skip_Line;
      declare

         N_Recs : array (1 .. N) of Rec;

         function Uniform_Boolean is new Random.Enumeration_Uniform(Boolean);

         use type Random_Numeric_Types.Random_Integer,
                  Random_Numeric_Types.Random_Real;

      begin
         for I in 1 .. N loop
            N_Recs(I) := (A => Natural(Random.Uniform(First => 0, Last => 100)),
                          B => Integer(Random.Uniform(First => -1012, Last => 1112)),
                          C => Float(Random.Uniform(First => -40.0, Last => 100.0)),
                          D => Float(Random.Uniform(First => 0.0, Last => 10.0)),
                          E => Uniform_Boolean,
                          F => Integer(Random.Uniform(First => -555, Last => 444)));
         end loop;
         CPU.Start_Counter;
         for I in 1 .. N loop
            Insert(Item => N_Recs(I), Into => Bag_1);
         end loop;
         Put_Line("CPU time used for insertion of " &
                  Image(N) & " items : " & Image(CPU.CPU_Time) & " seconds");
      end;
      Assign(Object => Bag_2, Value => Bag_1);
      Destroy(Bag_1);
      Swap(Bag_1, Bag_2);
      for I in 1 .. N loop
         case Zero_Four'(I mod 5) is
            when 0 =>
               Rec_Bags_Key_A.Remove(Key => Rec_Bags_Key_A.Min(Of_Bag => Bag_1), From => Bag_1);
            when 1 =>
               Rec_Bags_Key_B.Remove(Key => Rec_Bags_Key_B.Max(Of_Bag => Bag_1), From => Bag_1);
            when 2 =>
               Rec_Bags_Key_C.Remove(Key => Rec_Bags_Key_C.Min(Of_Bag => Bag_1), From => Bag_1);
            when 3 =>
               Rec_Bags_Key_D.Remove(Key => Rec_Bags_Key_D.Max(Of_Bag => Bag_1), From => Bag_1);
            when 4 =>
               Rec_Bags_Key_E.Remove(Key => Rec_Bags_Key_E.Min(Of_Bag => Bag_1), From => Bag_1);
         end case;
         if Card(Of_Bag => Bag_1) /= N - I then
            raise Test_Error;
         end if;
      end loop;
      if not Empty(Bag_1) then
         raise Test_Error;
      end if;
      Destroy(Bag_1);
      Put_Line("CPU time used for operations on " &
               Image(N) & " items : " & Image(CPU.CPU_Time) & " seconds");
   end;
   --
   Rec_Bags.Finalize;
end Test_Multiple_Key_Bags;
