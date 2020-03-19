-- Comparison of Floating Point Values for Equality
   ------------------------------------------------

-- Creation : 30-Mar-1992 by Mats Weber.


generic
   type Real is digits <>;
   Max_Relative_Difference : in Real :=  -- must be positive
      Real'Epsilon * 32.0;
function Float_Comparison (Left, Right : Real) return Boolean;
