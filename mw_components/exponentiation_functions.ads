-- GENERIC EXPONENTIATION FUNCTIONS
   --------------------------------

-- Creation : 17-NOV-1989 by Mats Weber, taken from package Math_Pack.


package Exponentiation_Functions is
--------------------------------

   generic
      type Number is private;
      One : in Number;              -- will be returned when the exponent is 0
      type Exponent is range <>;    -- must include 0 and abs N.
      with function "*" (X,Y : Number) return Number is <>;
      with function Inverse (X : Number) return Number is <>;
   function Exponentiation (X : Number; N : Exponent) return Number;


   generic
      type Number is private;
      One : in Number;              -- will be returned when the exponent is 0
      type Exponent is range <>;    -- muse include 0
      with function "*" (X,Y : Number) return Number is <>;
   function Natural_Exponentiation (X : Number; N : Exponent) return Number;
      -- Will raise Negative_Exponent if N < 0.


   Negative_Exponent : exception;

end Exponentiation_Functions;
