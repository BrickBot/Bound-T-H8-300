-- PACKAGE FOR RANDOM GENERATION
   -----------------------------

-- Revision : 16-SEP-1986 by Mats Weber, implemented as an instantiation of
--                                       generic package RANDOM_NUMBERS.
-- Revision : 11-DEC-1985 by Mats Weber.
-- Creation : 13-JUL-1985 by Mats Weber.

with Random_Numbers,
     The_Random_Generator,
     Random_Numeric_Types;

package Random is new Random_Numbers(Int                 => Random_Numeric_Types.Random_Integer,
                                     Real                => Random_Numeric_Types.Random_Real,
                                     Generator_Real      => The_Random_Generator.Random_Real,
                                     Generator_Uniform   => The_Random_Generator.Uniform,
                                     Reset_Generator     => The_Random_Generator.Reset,
                                     Randomize_Generator => The_Random_Generator.Randomize);
