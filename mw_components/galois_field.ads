-- GENERIC PACKAGE FOR HANDLING GALOIS FIELDS (GF(p^n))
   ----------------------------------------------------

-- Revision : 17-DEC-1987 by Mats Weber, replaced INTEGER with ZPZ_INTEGER and GF_INTEGER.
-- Revision :  6-NOV-1987 by Mats Weber, changed a few function names for clarity.
-- Revision :  5-JAN-1987 by Mats Weber, added functions TO_ZPZ and IN_ZPZ.
-- Revision :  5-NOV-1986 by Mats Weber, added generic function ENUMERATE_ELEMENTS.
-- Revision :  5-NOV-1986 by Mats Weber, put functions handling ZPZ into
--                                       package ZPZ_FIELD.
-- Revision :  3-NOV-1986 by Mats Weber, added functions ORDER and
--                                       MINIMAL_POLYNOMIAL.

-- Creation : 22-OCT-1986 by Mats Weber.


with ZpZ_Base_Type,
     GF_Base_Type,
     ZpZ_Field;

use ZpZ_Base_Type,
    GF_Base_Type;

generic
  P : in ZpZ_Positive;
  N : in Positive;
package Galois_Field is
--------------------

  -- This package uses the representation of GF(q) as a quotient
  -- Z/pZ[x]/(g(x)) where g(x) is any irreducible polynomial of
  -- degree n in Z/pZ[x].

  package Base_ZpZ_Field is new ZpZ_Field(P);
  use Base_ZpZ_Field;


  Q : constant GF_Positive := GF_Positive(P) ** N;  -- cardinality of the Galois field GF(q).

  GF_Polynomial : constant Base_ZpZ_Field.ZpZ_Polynomials.Polynomial := An_Irreducible(Deg => N);
    -- GF_POLYNOMIAL is the irreducible polynomial used to represent
    -- the Galois field.

  subtype Polynomial_N is Base_ZpZ_Field.ZpZ_Polynomials.Polynomial(0..N-1);


  type Element is private;  -- elements of the Galois field
  ------------

  type Element_Array is array (Positive range <>) of Element;

  Zero, One : constant Element;


  function To_Element (A : Base_ZpZ_Field.ZpZ) return Element;

  function In_ZpZ (A : Element) return Boolean;
    -- Checks if A is in the base field Z/pZ.

  function To_ZpZ (A : Element) return Base_ZpZ_Field.ZpZ;
    -- Returns the corresponding element of Z/pZ if A is in
    -- the base field. Raises NOT_IN_ZPZ otherwise.

  function To_Element (P : Base_ZpZ_Field.ZpZ_Polynomials.Polynomial) return Element;

  function To_Polynomial (A : Element) return Polynomial_N;
    -- Returns the representation of A as a polynomial.


  function "+" (A : Element) return Element;
  function "-" (A : Element) return Element;

  function "+" (A, B : Element) return Element;
  function "-" (A, B : Element) return Element;

  function "*" (A, B : Element) return Element;
  function "/" (A, B : Element) return Element;

  function "**" (A : Element; N : GF_Integer) return Element;
  function "**" (A : Element; N : Integer)    return Element;

  function Inverse (A : Element) return Element;

  function Order (A : Element) return GF_Positive;
    -- Returns the multiplicative order of A.

  function Minimal_Polynomial (A : Element) return Base_ZpZ_Field.ZpZ_Polynomials.Polynomial;
    -- Returns the minimal polynomial of A.

  function Is_Generator (A : Element) return Boolean;
    -- Tests if A is a generator of the multiplicative group of GF(q).

  function A_Generator return Element;
    -- Returns a generator of the multiplicative group of GF(q).

  function All_Generators return Element_Array;
    -- Returns all generators of the multiplicative group of GF(q).

  generic
    with procedure Action (E : in Element);
  procedure Enumerate_Elements;
    -- Enumerates all elements of GF(q).


  Attempt_To_Invert_Zero,                  -- raised by INVERSE(ELEMENT)
  Not_In_ZpZ                : exception;   -- raised by TO_ZPZ(ELEMENT)

private

  type Element is new Polynomial_N;

  Zero : constant Element := (others => 0);
  One  : constant Element := 1 & (1..Element'Last => 0);

end Galois_Field;
