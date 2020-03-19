-- GENERIC PACKAGE FOR HANDLING GALOIS FIELDS (GF(p^n))
   ----------------------------------------------------

-- Revision : 17-DEC-1987 by Mats Weber, replaced INTEGER with ZPZ_INTEGER and GF_INTEGER.
-- Revision :  6-NOV-1987 by Mats Weber, changed a few function names for clarity.

-- Creation : 20-JAN-1987 by Mats Weber.


with ZpZ_Base_Type,
     GF_Base_Type,
     Fast_GF_Base_Type,
     ZpZ_Field;

use ZpZ_Base_Type,
    GF_Base_Type;

generic
  P : in ZpZ_Positive;
  N : in Positive;
package Fast_Galois_Field is
-------------------------

  -- This package uses the representation of GF(q) as powers of
  -- a generator. Addition is implmented with a table.

  package Base_ZpZ_Field is new ZpZ_Field(P);


  Q : constant GF_Positive := GF_Positive(P) ** N;  -- cardinality of the Galois field GF(q).


  type Element is private;  -- elements of the Galois field
  ------------

  type Element_Array is array (Positive range <>) of Element;

  Zero, One : constant Element;


  function The_Generator return Element;
    -- Returns the generator used to represent GF(q).


  function To_Element (A : Base_ZpZ_Field.ZpZ) return Element;

  function In_ZpZ (A : Element) return Boolean;
    -- Checks if A is in the base field Z/pZ.

  function To_ZpZ (A : Element) return Base_ZpZ_Field.ZpZ;
    -- Returns the corresponding element of Z/pZ if A is in
    -- the base field. Raises NOT_IN_ZPZ otherwise.

  function GF_Polynomial return Base_ZpZ_Field.ZpZ_Polynomials.Polynomial;
    -- Returns the irreducible polynomial (of degree n) used to represent GF(q).

  function To_Polynomial (A : Element) return Base_ZpZ_Field.ZpZ_Polynomials.Polynomial;
    -- Returns the polynomial representing A.


  subtype Power is GF_Integer range 0..Q - 2;

  function Power_Of_Generator (A : Element) return Power;
    -- Returns N such that THE_GENERATOR ** N = A.
    -- Raises ZERO_IS_NOT_A_POWER if A = ZERO.


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
    with procedure Action(E : in Element);
  procedure Enumerate_Elements;
    -- Enumerates all elements of GF(q).


  Attempt_To_Invert_Zero,                  -- raised by INVERSE(ELEMENT)
  Zero_Is_Not_A_Power,                     -- raised by POWER_OF_GENERATOR(ELEMENT)
  Not_In_ZpZ                : exception;   -- raised by TO_ZPZ(ELEMENT)

private

  Q_1 : constant Fast_GF_Base_Type.Fast_GF_Positive :=
        Fast_GF_Base_Type.Fast_GF_Positive(Q - 1);

  type Element is new Fast_GF_Base_Type.Fast_GF_Integer range 0..Q_1;
    -- ZERO is represented as Q - 1

  Zero : constant Element := Element'Last;
  One  : constant Element := 0;

  pragma Inline (The_Generator, To_Element, In_ZpZ, To_ZpZ, Power_Of_Generator,
                 "+", "-", "*", "/", "**", Inverse);

end Fast_Galois_Field;
