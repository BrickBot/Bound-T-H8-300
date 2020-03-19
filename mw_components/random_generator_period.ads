-- GENERIC FUNCTION TO DETERMINE THE PERIOD OF A RANDOM NUMBER GENERATOR
   ---------------------------------------------------------------------

-- Creation : 18-AUG-1988 by Mats Weber.


generic
   type Random_Number_1 is private;
   type Random_Number_2 is private;
   with function Equal (X : Random_Number_1; Y : Random_Number_2) return Boolean;
   with function Random_1 return Random_Number_1;
   with function Random_2 return Random_Number_2;
      -- Two different instances of the same generator

   type Count is private;
   Zero : in Count;
   with function Succ (X : Count) return Count;
function Random_Generator_Period return Count;
--------------------------------
