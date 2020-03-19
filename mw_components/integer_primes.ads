-- INSTANCE OF PRIME_NUMBERS FOR INTEGER
   -------------------------------------

-- Revision : 23-FEB-1989 by Mats Weber, changes in PRIME_NUMBERS.

-- Creation : 27-0CT-1986 by Mats Weber.


with Prime_Numbers,
     Integer_Primes_Parameters;

package Integer_Primes is
----------------------
   new Prime_Numbers(Int        => Integer,
                     Zero       => 0,
                     One        => 1,
                     Sqrt       => Integer_Primes_Parameters.Sqrt,
                     To_Integer => Integer_Primes_Parameters.To_Integer);
