-- PACKAGE FOR RANDOM GENERATION RETURNING VALUES OF TYPES IN STANDARD
   -------------------------------------------------------------------

-- Creation : 12-MAR-1991 by Mats Weber.

with Random_Numbers,
     The_Random_Generator;

package Standard_Random is
   new Random_Numbers(Int                 => Integer,
                      Real                => Float,
                      Generator_Real      => The_Random_Generator.Random_Real,
                      Generator_Uniform   => The_Random_Generator.Uniform,
                      Reset_Generator     => The_Random_Generator.Reset,
                      Randomize_Generator => The_Random_Generator.Randomize);
