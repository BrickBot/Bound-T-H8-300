-- GENERIC PACKAGE FOR POLYNOMIALS
   -------------------------------

-- Revision : 26-MAR-1986 by Mats Weber, added generic formal type INDEX.
-- Revision :  3-NOV-1986 by Mats Weber, added generic function POLYNOMIAL_EVALUATION.

-- Creation : 21-OCT-1986 by Mats Weber.


generic
  type Index is range <>;        -- must include -1
  type Coefficient is private;
  Zero, One : in Coefficient;
  with function "+" (X : Coefficient) return Coefficient is <>;
  with function "-" (X : Coefficient) return Coefficient is <>;
  with function "+" (X, Y : Coefficient) return Coefficient is <>;
  with function "-" (X, Y : Coefficient) return Coefficient is <>;
  with function "*" (X, Y : Coefficient) return Coefficient is <>;
  with function "/" (X, Y : Coefficient) return Coefficient is <>;
package Polynomials is
-------------------

  subtype Degree is Index range -1..Index'Last;
    -- The degree of the zero polynomial is -1.

  subtype Nonnegative_Degree is Index range 0..Index'Last;

  type Polynomial is array (Nonnegative_Degree range <>) of Coefficient;
    -- The index 0 must always be present in the representation
    -- of a polynomial, except for the zero polynomial; however
    -- trailing ZEROs are allowed.

  Zero_Polynomial : constant Polynomial := (0..-1 => Zero);

  function Equal (P, Q : Polynomial) return Boolean;
    -- Tests if P and Q represent the same polynomial.

  procedure Assign (Object : out Polynomial; Value : in Polynomial);
    -- OBJECT := VALUE; (OBJECT is filled with ZEROs if necessary)


  function "+" (P : Polynomial) return Polynomial;
  function "-" (P : Polynomial) return Polynomial;

  function "+" (P, Q : Polynomial) return Polynomial;
  function "-" (P, Q : Polynomial) return Polynomial;

  function "*"   (P, Q : Polynomial) return Polynomial;
  function "/"   (P, Q : Polynomial) return Polynomial;
  function "mod" (P, Q : Polynomial) return Polynomial;

  function "*" (X : Coefficient; P : Polynomial) return Polynomial;
  function "*" (P : Polynomial; X : Coefficient) return Polynomial;
  function "/" (P : Polynomial; X : Coefficient) return Polynomial;

  function "**" (P : Polynomial; N : Natural) return Polynomial;

  function Deg (P : Polynomial) return Degree;

  generic
    type Item is private;
    with function To_Item (A : Coefficient) return Item;
    with function "*" (X, Y : Item) return Item is <>;
    with function "+" (X, Y : Item) return Item is <>;
  function Polynomial_Evaluation (P : Polynomial; X : Item) return Item;
    -- Evaluates P(X).

end Polynomials;
