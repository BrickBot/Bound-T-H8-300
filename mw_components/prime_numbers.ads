-- GENERIC PACKAGE FOR PRIME NUMBERS AND RELATED OPERATIONS
   --------------------------------------------------------

-- Revision : 23-FEB-1989 by Mats Weber, made generic fromal type INT a private type.
-- Revision : 23-NOV-1987 by Mats Weber, removed ALL_DIVISORS and modified DIVISORS.
-- Revision : 23-FEB-1987 by Mats Weber, added function DIVISORS.
-- Revision :  7-DEC-1986 by Mats Weber, added GCD and LCM of a list of numbers.
-- Revision : 30-OCT-1986 by Mats Weber, added function GCD.

-- Creation : 26-OCT-1986 by Mats Weber.


generic
  type Int is private;
  Zero, One : in Int;

  with function "abs" (X : Int)    return Int is <>;
  with function "-"   (X : Int)    return Int is <>;
  with function "+"   (X, Y : Int) return Int is <>;
  with function "-"   (X, Y : Int) return Int is <>;
  with function "*"   (X, Y : Int) return Int is <>;
  with function "/"   (X, Y : Int) return Int is <>;
  with function "mod" (X, Y : Int) return Int is <>;
  with function Equal (X, Y : Int) return Boolean is "=";
  with function "<"   (X, Y : Int) return Boolean is <>;
  with function "<="  (X, Y : Int) return Boolean is <>;

  with function Sqrt (X : Int) return Int is <>;   -- must return the integer part of the square root of X
  with function To_Integer (X : Int) return Integer is <>;
package Prime_Numbers is
---------------------

  type Int_Array is array (Positive range <>) of Int;

  type Power is
    record
      Base     : Int;
      Exponent : Natural;
    end record;

  type Array_Of_Power is array (Positive range <>) of Power;


  function Smallest_Divisor (N : Int) return Int;
    -- Returns the smallest divisor (except 1) of N.

  function Prime (N : Int) return Boolean;
    -- Tests if N is prime.

  function Primes (From, To : Int) return Int_Array;
    -- Returns all prime numbers between FROM and TO.

  function Factorization (N : Int) return Array_Of_Power;
    -- Returns the decomposition of N in a product of prime numbers.

  function Divisors (N : Int; Include_1, Include_N : Boolean := False) return Int_Array;
    -- Returns the divisors of N in an ordered list.
    -- 1 is included if INCLUDE_1 is TRUE.
    -- N is included if INCLUDE_N is TRUE.

  function GCD (M, N : Int)          return Int;
  function GCD (Numbers : Int_Array) return Int;
    -- Returns the GCD of the given numbers.

  function LCM (M, N : Int)          return Int;
  function LCM (Numbers : Int_Array) return Int;
    -- Returns the LCM of the given numbers.

  function Euler (N : Int) return Int;
    -- Returns the number of integers in the range 1..N-1 that are
    -- relatively prime to N.

  Negative_Number : exception;

end Prime_Numbers;
