-- GENERIC PACKAGE FOR RANDOM GENERATION
   -------------------------------------

-- Revision : 12-FEB-1988 by Mats Weber, added generic function ENUMERATION_UNIFORM.
-- Revision : 16-SEP-1985 by Mats Weber, renamed INT_RANDOM to UNIFORM,
--                                       added function PROBABILITY.
-- Revision : 11-DEC-1985 by Mats Weber.

-- Creation : 13-JUL-1985 by Mats Weber.


generic
  type Int is range <>;
  type Real is digits <>;
  type Generator_Real is digits <>;
  with function Generator_Uniform return Generator_Real;
    -- must return values uniformly distributed in [0,1).
  with procedure Reset_Generator;
  with procedure Randomize_Generator;
package Random_Numbers is
----------------------

  function Uniform (First, Last : Int) return Int;
    -- Returns a uniformly distributed random integer in the range FIRST..LAST

  generic
    type Enumeration is (<>);
  function Enumeration_Uniform return Enumeration;
    -- Returns a random value uniformly distributed in ENUMERATION'FIRST..ENUMERATION'LAST


  subtype Real_0_1 is Real range 0.0..1.0;

  function Uniform return Real_0_1;
    -- Returns a random number uniformly distributed in [0,1)

  function Uniform (First, Last : Real) return Real;
    -- Returns a random number uniformly distributed in [FIRST, LAST)

  function Normal return Real;
    -- Returns a N(0,1)-distributed random number

  function Exponential return Real;
    -- Returns a number following an exponential distribution with mean 1

  function Probability (P : Real_0_1) return Boolean;
    -- Returns TRUE with probability P

  procedure Reset;
    -- Resets the generator's seed to its initial value

  procedure Randomize;
    -- Sets the generator's seed to a time-dependant value


  pragma Inline(Uniform, Enumeration_Uniform, Probability, Reset, Randomize);

end Random_Numbers;
