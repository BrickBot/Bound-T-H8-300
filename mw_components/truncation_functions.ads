-- TRUNCATION, FLOOR AND CEILING FUNCTIONS
   ---------------------------------------

-- Creation : 17-NOV-1989 by Mats Weber, taken from package Utilities.


package Truncation_Functions is
----------------------------

   generic
      type Real is digits <>;
      type Int is range <>;
   function Float_Floor (X : Real) return Int;

   generic
      type Real is digits <>;
      type Int is range <>;
   function Float_Ceiling (X : Real) return Int;


   generic
      type Real is delta <>;
      type Int is range <>;
   function Fixed_Floor (X : Real) return Int;

   generic
      type Real is delta <>;
      type Int is range <>;
   function Fixed_Ceiling (X : Real) return Int;


   pragma Inline(Float_Floor, Float_Ceiling,
                 Fixed_Floor, Fixed_Ceiling);

end Truncation_Functions;
