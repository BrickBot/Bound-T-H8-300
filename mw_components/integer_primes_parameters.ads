-- GENERIC PARAMETERS FOR INSTANCE INTEGER_PRIMES
   ----------------------------------------------

-- Creation : 23-FEB-1989 by Mats Weber.


with Square_Root_Functions;

package Integer_Primes_Parameters is
---------------------------------

   function To_Integer (X : Integer) return Integer;

   function Sqrt is new Square_Root_Functions.Integer_Sqrt(Int => Integer);

end Integer_Primes_Parameters;
