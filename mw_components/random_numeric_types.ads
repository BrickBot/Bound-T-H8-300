-- NUMERIC TYPES FOR STANDARD INSTANCE OF PACKAGE RANDOM_NUMBERS
   -------------------------------------------------------------

-- Creation :  3-DEC-1987 by Mats Weber.


with The_Random_Generator;

package Random_Numeric_Types is
----------------------------

  type Random_Integer is range The_Random_Generator.Random_Integer_Base'First..
                               The_Random_Generator.Random_Integer_Base'Last;

  type Random_Real is digits The_Random_Generator.Random_Real'Digits;

end Random_Numeric_Types;
