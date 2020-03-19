-- SQUARE ROOT FUNCTIONS
   ---------------------

-- Creation : 17-NOV-1989 by Mats Weber, taken from package Math_Pack.


package Square_Root_Functions is
-----------------------------

   generic
      type Int is range <>;
   function Integer_Sqrt (X : Int) return Int;

   generic
      type Real is delta <>;
   function Fixed_Sqrt (X : Real) return Real;

   Sqrt_Of_Negative_Value : exception;

end Square_Root_Functions;
