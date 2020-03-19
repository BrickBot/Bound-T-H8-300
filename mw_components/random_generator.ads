-- RANDOM NUMBER GENERATOR
   -----------------------

-- Creation : 24-NOV-1987 by Mats Weber.


generic
package Random_Generator is
------------------------

   Modulus : constant := 2**31 - 1;

   type Random_Integer_Base is range - (Modulus - 1) .. Modulus - 1;
   subtype Random_Integer is Random_Integer_Base range 1 .. Modulus - 1;

   type Random_Real is digits Random_Integer_Base'Width - 1;
   subtype Random_Real_0_1 is Random_Real range 0.0 .. 1.0;

   type Random_Seed is private;

   Initial_Seed : constant Random_Seed;

   function To_Seed (Number : Random_Integer) return Random_Seed;
   function To_Number (Seed : Random_Seed) return Random_Integer;


   function Uniform return Random_Integer;
      -- Returns a random integer uniformly distributed in
      -- RANDOM_INTEGER's range, bounds inclusive.
      -- X(N+1) := 16807 * X(N) mod (2**31 - 1).

   function Uniform return Random_Real_0_1;
      -- Returns a random number uniformly distributed in [0,1).

   function Current_Seed return Random_Seed;
      -- Returns the generator's current seed.

   procedure Set_Seed (Seed : in Random_Seed);
      -- Sets the generator's seed.

   procedure Randomize;
      -- Sets the generator's seed to a time dependant value.

   procedure Reset;
      -- Resets the generator's seed to its initial value.

private

   type Random_Seed is new Random_Integer;

   Initial_Seed : constant Random_Seed := 59823476;


   pragma Inline(Uniform,
                 Current_Seed, Set_Seed, Reset,
                 To_Seed, To_Number);

end Random_Generator;
