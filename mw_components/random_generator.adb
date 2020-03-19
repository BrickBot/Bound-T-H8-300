-- RANDOM NUMBER GENERATOR
   -----------------------

-- Creation : 25-NOV-1987 by Mats Weber.


with Calendar;

package body Random_Generator is
-----------------------------

   A : constant := 16807;
   B : constant := 127773;
   C : constant := Modulus - A * B;   -- C = 2836;

   Seed : Random_Seed := Initial_Seed;

   Divisor : constant Random_Real :=
             Random_Real(Random_Integer'Last - Random_Integer'First + 1);


   function To_Seed (Number : Random_Integer) return Random_Seed is
   begin
      return Random_Seed(Number);
   end;


   function To_Number (Seed : Random_Seed) return Random_Integer is
   begin
      return Random_Integer(Seed);
   end;


   function Uniform return Random_Integer is
      -- SEED := A * SEED mod MODULUS;
      -- return SEED;

      A_Seed_Mod_B : constant Random_Integer_Base := A * (Random_Integer_Base(Seed) mod B);
      Seed_Div_B_C : constant Random_Integer_Base := Random_Integer_Base(Seed) / B * C;

   begin
      if A_Seed_Mod_B >= Seed_Div_B_C then
         Seed := Random_Seed(A_Seed_Mod_B - Seed_Div_B_C);
      else
         Seed := Random_Seed(A_Seed_Mod_B + (Modulus - Seed_Div_B_C));
      end if;
      return Random_Integer(Seed);
   end Uniform;


   function Uniform return Random_Real_0_1 is
   begin
      return Random_Real(Uniform - Random_Integer'First) / Divisor;
   end;


   function Current_Seed return Random_Seed is
   begin
      return Seed;
   end;


   procedure Set_Seed (Seed : in Random_Seed) is
   begin
      Random_Generator.Seed := Seed;
   end;


   procedure Randomize is

      use Calendar;

      Max_Term_Value      : constant Random_Integer := Random_Integer'Last / 4;

      type Duration_Multiplier is delta 1.0 range 0.0 .. 1.0 * Random_Integer'Pos(Max_Term_Value);

      Seconds_Multiplier  : constant Duration_Multiplier :=
                            Duration_Multiplier(Duration_Multiplier(Max_Term_Value) /
                                                Duration_Multiplier(Day_Duration'Last));

      Day_Multiplier      : constant Random_Integer :=
                            Max_Term_Value / Random_Integer(Day_Number'Last);

      Month_Multiplier    : constant Random_Integer :=
                            Max_Term_Value / Random_Integer(Month_Number'Last);

      Year_Multiplier     : constant Random_Integer :=
                            Max_Term_Value /
                            Random_Integer(Year_Number'Last - Year_Number'First + 1);

      Now                 : constant Time := Clock;

   begin
      Seed := Random_Seed(Random_Integer_Base(Seconds(Now) * Seconds_Multiplier) +
                          Random_Integer(Day(Now)) * Day_Multiplier +
                          Random_Integer(Month(Now)) * Month_Multiplier +
                          Random_Integer(Year(Now) - Year_Number'First + 1) * Year_Multiplier);
   end Randomize;


   procedure Reset is
   begin
      Seed := Initial_Seed;
   end;

end Random_Generator;
