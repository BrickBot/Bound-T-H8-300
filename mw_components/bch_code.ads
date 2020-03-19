-- GENERIC PACKAGE FOR THE STUDY OF BCH CODES
   ------------------------------------------

-- Revision :  6-JUN-1989 by Mats Weber, made function APPROXIMATIVE_WEIGHT_DISTRIBUTION generic
--                                       with respect to RANDOM and moved exception
--                                       INCONSISTENT_PARAMETERS to package BCH_CODE_EXCEPTIONS.
-- Revision : 23-JAN-1987 by Mats Weber, added generic parameters to allow use with
--                                       different implementations of Galois fields.
-- Revision : 28-DEC-1986 by Mats Weber, added subtype WORD_INDEX.
-- Revision :  1-DEC-1986 by Mats Weber, added function WEIGHT_DISTRIBUTION.
-- Revision : 19-NOV-1986 by Mats Weber, added function ASSOCIATED_POLYNOMIAL.

-- Creation :  6-NOV-1986 by Mats Weber.


with ZpZ_Base_Type,
     GF_Base_Type;

generic
  P : in ZpZ_Base_Type.ZpZ_Positive;
  Q : in GF_Base_Type.GF_Positive;
  N : in Positive;
  T : in Positive;   -- one must have P**N = Q, P prime and 1 <= T < Q.

  type ZpZ is range <>;     -- one must have ZPZ'FIRST = 0 and ZPZ'LAST = P - 1
  with function "+" (A : ZpZ) return ZpZ is <>;
  with function "-" (A : ZpZ) return ZpZ is <>;
  with function "+" (A,B : ZpZ) return ZpZ is <>;
  with function "-" (A,B : ZpZ) return ZpZ is <>;
  with function "*" (A,B : ZpZ) return ZpZ is <>;
  with function "/" (A,B : ZpZ) return ZpZ is <>;

  type Element is private;    -- elements of the Galois field with Q elements.
  Zero, One,
  Generator   : in Element;   -- GENERATOR must be any generator of the multiplicative group
  with function "+" (A : Element) return Element is <>;
  with function "-" (A : Element) return Element is <>;
  with function "+" (A,B : Element) return Element is <>;
  with function "-" (A,B : Element) return Element is <>;
  with function "*" (A,B : Element) return Element is <>;
  with function "/" (A,B : Element) return Element is <>;

  type Polynomial is array (Natural range <>) of ZpZ;
  Zero_Polynomial : in Polynomial;
  with function "+" (P : Polynomial) return Polynomial is <>;
  with function "-" (P : Polynomial) return Polynomial is <>;
  with function "+" (P,Q : Polynomial) return Polynomial is <>;
  with function "-" (P,Q : Polynomial) return Polynomial is <>;
  with function "*" (P,Q : Polynomial) return Polynomial is <>;
  with function "/" (P,Q : Polynomial) return Polynomial is <>;
  with function "mod" (P,Q : Polynomial) return Polynomial is <>;
  with function Deg (P : Polynomial) return Integer is <>;
  with procedure Assign (Object : out Polynomial; Value : in Polynomial) is <>;

  with function To_ZpZ (N : ZpZ_Base_Type.ZpZ_Integer) return ZpZ is <>;
  with function Minimal_Polynomial (A : Element) return Polynomial is <>;

  -- The exception BCH_CODE_EXCEPTIONS.INCONSISTENT_PARAMETERS will be raised
  -- if some generic parameters do not satisfy the indicated constraints.
package BCH_Code is
----------------

  GF_Generator : constant Element := Generator;
    -- Generator of the multiplicative group of GF(q) used to
    -- represent the code.


  subtype Word_Polynomial is Polynomial(0..Integer(Q) - 2);

  type Word is new Word_Polynomial;
  ---------

  subtype Word_Index is Natural range Word'Range;

  subtype Hamming_Weight is Natural range 0..Integer(Q) - 1;


  type Array_Of_Word is array (Positive range <>) of Word;

  type Distribution is array (Hamming_Weight) of Natural;


  function In_Code (W : Word) return Boolean;
    -- Checks if W is a word of the code.
    -- (i.e. if GENERATOR_POLYNOMIAL divides W's polynomial).

  function Weight (W : Word) return Hamming_Weight;
    -- Returns the Hamming weight of W.

  function Distance (W1, W2 : Word) return Hamming_Weight;
    -- Returns the Hamming distance of W1 and W2.

  function To_Word (A : Polynomial) return Word;
    -- Returns the word corresponding to the polynomial A(x).

  function "+" (W1, W2 : Word) return Word;
  function "-" (W1, W2 : Word) return Word;

  function "*" (A : ZpZ; W : Word) return Word;
  function "*" (W : Word; A : ZpZ) return Word;
  function "/" (W : Word; A : ZpZ) return Word;


  function Generator_Polynomial return Polynomial;
    -- Returns the polynomial used to generate the code.

  function Associated_Polynomial return Polynomial;
    -- Returns the polynomial (x^(q-1)-1)/g(x) where g(x) is the
    -- generator polynomial.

  function Hamming_Number return Hamming_Weight;
    -- Returns the smallest weight of all non zero words of the code.

  function Errors_Corrected return Natural;
    -- Returns the number of errors the code can correct.

  function Errors_Detected return Natural;
    -- Returns the number of errors the code can detect.

  function Weight_Distribution return Distribution;
    -- Returns the weight distribution of the code.
    -- (The number of words of each weight).

  generic
    with function Uniform return ZpZ;
      -- must return a uniformly distributed value of type ZPZ.
  function Approximative_Weight_Distribution (N_Words : Natural) return Distribution;
    -- Returns an approximation of the weight distribution of the code,
    -- evaluated with N_WORDS randomly selected codewords.

  function Canonical_Representation return Boolean;
    -- Checks if BCH(p,n,t,a) is a canonical representation.

  function BCH_Bound return Positive;
    -- Returns the value of T generating the same code, but such
    -- that the representation is canonical.

  function Dimension return Positive;
    -- Returns the dimension of the code.

  function Base return Array_Of_Word;
    -- Returns a base of the code's vector space.

end BCH_Code;
