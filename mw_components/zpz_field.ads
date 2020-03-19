-- GENERIC PACKAGE FOR HANDLING THE FIELD Z/pZ
   -------------------------------------------

-- Revision :  5-JUN-1989 by Mats Weber, moved exception P_NOT_PRIME to
--                                       package ZPZ_FIELD_EXCEPTIONS.
-- Revision : 17-DEC-1987 by Mats Weber, replaced INTEGER with ZPZ_INTEGER.
-- Revision :  6-NOV-1987 by Mats Weber, changed a few function names for clarity.
-- Revision : 15-JAN-1987 by Mats Weber, use package ZPZ_BASE_TYPE.

-- Creation :  5-NOV-1986 by Mats Weber.


with ZpZ_Base_Type,
     Polynomials;

use ZpZ_Base_Type;

generic
  P : in ZpZ_Positive;       -- The exception ZPZ_FIELD_EXCEPTIONS.P_NOT_PRIME
package ZpZ_Field is         -- will be raised if P is not prime.
-----------------

  type ZpZ is new ZpZ_Integer range 0..ZpZ_Integer(P - 1);
    -- elements of the field Z/pZ

  type ZpZ_Array is array (Positive range <>) of ZpZ;


  function "+" (A : ZpZ) return ZpZ;
  function "-" (A : ZpZ) return ZpZ;

  function "+" (A, B : ZpZ) return ZpZ;
  function "-" (A, B : ZpZ) return ZpZ;

  function "*" (A, B : ZpZ) return ZpZ;
  function "/" (A, B : ZpZ) return ZpZ;

  function "**" (A : ZpZ; N : ZpZ_Integer) return ZpZ;
  function "**" (A : ZpZ; N : Integer)     return ZpZ;

  function Inverse (A : ZpZ) return ZpZ;

  function To_ZpZ (N : ZpZ_Integer) return ZpZ;
    -- Returns N modulo P.

  function Order (A : ZpZ) return ZpZ_Positive;
    -- Returns the multiplicative order of A.

  function Is_Generator (A : ZpZ) return Boolean;
    -- Tests if A is a generator of the multiplicative group of Z/pZ.

  function A_Generator return ZpZ;
    -- Returns a generator of the multiplicative group of Z/pZ.

  function All_Generators return ZpZ_Array;
    -- Returns all generators of the multiplicative group of Z/pZ.


  package ZpZ_Polynomials is new Polynomials(Index       => Integer,
                                             Coefficient => ZpZ,
                                             Zero        => 0,
                                             One         => 1);


  function Is_Irreducible (P : ZpZ_Polynomials.Polynomial) return Boolean;
    -- Tests if P is irreducible in Z/pZ[x]

  function An_Irreducible (Deg : ZpZ_Polynomials.Degree)
    return ZpZ_Polynomials.Polynomial;
    -- Returns an irreducible polynomial of degree DEG.

  generic
    with procedure Action (R : in ZpZ_Polynomials.Polynomial);
  procedure Enumerate_Polynomials (Max_Deg : in ZpZ_Polynomials.Degree);
    -- Enumerates all polynomials of degree <= MAX_DEG in Z/pZ[x].

  generic
    with procedure Action (P : in ZpZ_Polynomials.Polynomial);
  procedure Enumerate_Irreducibles (Deg : in ZpZ_Polynomials.Degree);
    -- Enumerates all irreducible polynomials of degree DEG in Z/pZ[x].


  Attempt_To_Invert_0 : exception;   -- raised by INVERSE(ZPZ)


  pragma Inline("+", "-", "*", "/", Inverse, To_ZpZ);

end ZpZ_Field;
