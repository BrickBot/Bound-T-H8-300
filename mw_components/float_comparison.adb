-- Comparison of Floating Point Values for Equality
   ------------------------------------------------

-- Creation : 30-Mar-1992 by Mats Weber.


with Min_Max_Functions;

function Float_Comparison (Left, Right : Real) return Boolean is

   function Max is new Min_Max_Functions.Maximum(Real);

begin
   return abs (Right - Left) <=
          Max(abs Left, abs Right) * Max_Relative_Difference;
end Float_Comparison;
