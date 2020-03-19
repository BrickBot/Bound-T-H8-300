-- GENERIC PACKAGE FOR MATRIX OPERATIONS
   -------------------------------------

-- Creation : 20-SEP-1986 by Mats Weber.

generic
  type Index is (<>);
  type Scalar is private;
  Zero, One  : in Scalar;
  with function "+" (X : Scalar) return Scalar is <>;
  with function "-" (X : Scalar) return Scalar is <>;
  with function "+" (X,Y : Scalar) return Scalar is <>;
  with function "-" (X,Y : Scalar) return Scalar is <>;
  with function "*" (X,Y : Scalar) return Scalar is <>;
  with function "/" (X,Y : Scalar) return Scalar is <>;
package Matrices is

  type Vector is array (Index range <>) of Scalar;
  type Matrix is array (Index range <>, Index range <>) of Scalar;

  function "+" (V : Vector) return Vector;
  function "-" (V : Vector) return Vector;

  function "+" (V,W : Vector) return Vector;
  function "-" (V,W : Vector) return Vector;

  function "*" (V,W : Vector) return Scalar;

  function "*" (X : Scalar; V : Vector) return Vector;
  function "*" (V : Vector; X : Scalar) return Vector;
  function "/" (V : Vector; X : Scalar) return Vector;


  function Row    (A : Matrix; I : Index) return Vector;
  function Column (A : Matrix; J : Index) return Vector;

  procedure Set_Row    (A : in out Matrix; I : in Index; V : in Vector);
  procedure Set_Column (A : in out Matrix; J : in Index; V : in Vector);


  function "+" (A : Matrix) return Matrix;
  function "-" (A : Matrix) return Matrix;

  function "+" (A,B : Matrix) return Matrix;
  function "-" (A,B : Matrix) return Matrix;
  function "*" (A,B : Matrix) return Matrix;
  function "/" (A,B : Matrix) return Matrix;

  function "**" (A : Matrix; N : Integer) return Matrix;

  function "*" (X : Scalar; A : Matrix) return Matrix;
  function "*" (A : Matrix; X : Scalar) return Matrix;
  function "/" (A : Matrix; X : Scalar) return Matrix;
  function "/" (X : Scalar; A : Matrix) return Matrix;

  function "*" (A : Matrix; V : Vector) return Vector;
  function "*" (V : Vector; A : Matrix) return Vector;

  function Identity (First, Last : Index) return Matrix;
    -- Returns the identity matrix with bounds (FIRST..LAST, FIRST..LAST)

  function Transposed (A : Matrix) return Matrix;


  function Det     (A : Matrix) return Scalar;
  function Inverse (A : Matrix) return Matrix;

  function Linear_System_Solution (A : Matrix; B : Vector) return Vector;
    -- Returns the solution of the system A*X = B.

  function Linear_System_Solution (A : Matrix; B : Matrix) return Matrix;
    -- Returns a matrix X such that A*X = B.


  type Permutation is array (Index range <>) of Index;

  function Permute        (V : Vector; P : Permutation) return Vector;
  function Row_Permute    (A : Matrix; P : Permutation) return Matrix;
  function Column_Permute (A : Matrix; P : Permutation) return Matrix;
  function Permute        (A : Matrix; On_Rows, On_Columns : Permutation) return Matrix;

  procedure LU_Decompose (A        : in out Matrix;
                          P        : out Permutation;
                          P_Odd,
                          Permuted : out Boolean);
    -- Decomposes A in a product of two matrices L and U such that L*U = A,
    -- where L is a lower triangular matrix with all diagonal
    -- coefficients equal to ONE and U is an upper triangular matrix.
    -- (L and U are stored in A with zero coefficients omitted)
    -- P is the permutation of the rows of A necessary to achieve the
    -- decomposition in the case of zero pivots, P_ODD is set to TRUE if P
    -- is an odd permutation and PERMUTED is set to FALSE if P is the
    -- identity permutation.

  function Triangular_System_Solution (A : Matrix; B : Vector) return Vector;
    -- Returns the solution of the system A'*X = B, where A' is equal to A
    -- but with all coefficients under the diagonal equal to ZERO.

  function LU_System_Solution (LU : Matrix; B : Vector) return Vector;
    -- Returns the solution of the system L*U*X = B, where LU is the
    -- LU decomposition of some matrix.
    -- (In some cases, B will have to be permuted first)


  Index_Error,
  Bound_Mismatch,
  Matrix_Singular       : exception;

end Matrices;
