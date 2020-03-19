-- GENERIC PACKAGE FOR MATRIX OPERATIONS
   -------------------------------------

-- Creation : 20-SEP-1986 by Mats Weber.

with Exchange,
     Exponentiation_Functions;

package body Matrices is
---------------------

  function Same_Bounds (V, W : Vector) return Boolean is
  begin
    return V'First = W'First and V'Last = W'Last;
  end Same_Bounds;


  function Same_Bounds (A, B : Matrix) return Boolean is
  begin
    return A'First(1) = B'First(1) and A'Last(1) = B'Last(1) and
           A'First(2) = B'First(2) and A'Last(2) = B'Last(2);
  end Same_Bounds;


  function Same_Bounds (A : Matrix) return Boolean is
  begin
    return A'First(1) = A'First(2) and A'Last(1) = A'Last(2);
  end Same_Bounds;


  function "+" (V : Vector) return Vector is

    Result : Vector(V'Range);

  begin
    for I in V'Range loop
      Result(I) := +V(I);
    end loop;
    return Result;
  end "+";


  function "-" (V : Vector) return Vector is

    Result : Vector(V'Range);

  begin
    for I in V'Range loop
      Result(I) := -V(I);
    end loop;
    return Result;
  end "-";


  function "+" (V, W : Vector) return Vector is

    Result : Vector(V'Range);

  begin
    if Same_Bounds(V, W) then
      for I in V'Range loop
        Result(I) := V(I) + W(I);
      end loop;
      return Result;
    else
      raise Bound_Mismatch;
    end if;
  end "+";


  function "-" (V, W : Vector) return Vector is

    Result : Vector(V'Range);

  begin
    if Same_Bounds(V, W) then
      for I in V'Range loop
        Result(I) := V(I) - W(I);
      end loop;
      return Result;
    else
      raise Bound_Mismatch;
    end if;
  end "-";


  function "*" (V, W : Vector) return Scalar is

    Result : Scalar := Zero;

  begin
    if Same_Bounds(V, W) then
      for I in V'Range loop
        Result := Result + V(I) * W(I);
      end loop;
      return Result;
    else
      raise Bound_Mismatch;
    end if;
  end "*";


  function "*" (X : Scalar; V : Vector) return Vector is

    Result : Vector(V'Range);

  begin
    for I in V'Range loop
      Result(I) := X * V(I);
    end loop;
    return Result;
  end "*";


  function "*" (V : Vector; X : Scalar) return Vector is

    Result : Vector(V'Range);

  begin
    for I in V'Range loop
      Result(I) := V(I) * X;
    end loop;
    return Result;
  end "*";


  function "/" (V : Vector; X : Scalar) return Vector is

    Result : Vector(V'Range);

  begin
    for I in V'Range loop
      Result(I) := V(I) / X;
    end loop;
    return Result;
  end "/";


  function Row (A : Matrix; I : Index) return Vector is

    Result : Vector(A'Range(2));

  begin
    if I not in A'Range(1) then
      raise Index_Error;
    end if;
    for J in A'Range(2) loop
      Result(J) := A(I, J);
    end loop;
    return Result;
  end Row;


  function Column (A : Matrix; J : Index) return Vector is

    Result : Vector(A'Range(1));

  begin
    if J not in A'Range(2) then
      raise Index_Error;
    end if;
    for I in A'Range(1) loop
      Result(I) := A(I, J);
    end loop;
    return Result;
  end Column;


  procedure Set_Row (A : in out Matrix; I : in Index; V : in Vector) is
  begin
    if A'First(2) /= V'First or A'Last(2) /= V'Last then
      raise Bound_Mismatch;
    end if;
    for J in A'Range(2) loop
      A(I, J) := V(J);
    end loop;
  end Set_Row;


  procedure Set_Column (A : in out Matrix; J : in Index; V : in Vector) is
  begin
    if A'First(1) /= V'First or A'Last(1) /= V'Last then
      raise Bound_Mismatch;
    end if;
    for I in A'Range(1) loop
      A(I, J) := V(I);
    end loop;
  end Set_Column;


  function "+" (A : Matrix) return Matrix is

    Result : Matrix(A'Range(1), A'Range(2));

  begin
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Result(I, J) := +A(I, J);
      end loop;
    end loop;
    return Result;
  end "+";


  function "-" (A : Matrix) return Matrix is

    Result : Matrix(A'Range(1), A'Range(2));

  begin
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Result(I, J) := -A(I, J);
      end loop;
    end loop;
    return Result;
  end "-";


  function "+" (A, B : Matrix) return Matrix is

    Result : Matrix(A'Range(1), A'Range(2));

  begin
    if Same_Bounds(A, B) then
      for I in A'Range(1) loop
        for J in A'Range(2) loop
          Result(I, J) := A(I, J) + B(I, J);
        end loop;
      end loop;
      return Result;
    else
      raise Bound_Mismatch;
    end if;
  end "+";


  function "-" (A, B : Matrix) return Matrix is

    Result : Matrix(A'Range(1), A'Range(2));

  begin
    if Same_Bounds(A, B) then
      for I in A'Range(1) loop
        for J in A'Range(2) loop
          Result(I, J) := A(I, J) - B(I, J);
        end loop;
      end loop;
      return Result;
    else
      raise Bound_Mismatch;
    end if;
  end "-";


  function "*" (A, B : Matrix) return Matrix is

    subtype Row_A    is Index range A'Range(1);
    subtype Column_B is Index range B'Range(2);

    Result : Matrix(Row_A, Column_B);


    function Row_By_Column (I : Row_A; J : Column_B) return Scalar is

      Result : Scalar := Zero;

    begin
      for K in A'Range(2) loop
        Result := Result + A(I, K) * B(K, J);
      end loop;
      return Result;
    end;

  begin
    if A'First(2) = B'First(1) and A'Last(2) = B'Last(1) then
      for I in Row_A loop
        for J in Column_B loop
          Result(I, J) := Row_By_Column(I, J);
        end loop;
      end loop;
      return Result;
    else
      raise Bound_Mismatch;
    end if;
  end "*";


  function "/" (A, B : Matrix) return Matrix is
  begin
    return A * Inverse(B);
  end "/";


  function "**" (A : Matrix; N : Integer) return Matrix is

    subtype Matrix_A is Matrix(A'Range(1), A'Range(2));

    function "**" is
      new Exponentiation_Functions.Exponentiation(Matrix_A,
                                                  Exponent => Integer,
                                                  One      => Identity(A'First(1),
                                                                       A'Last(1)));

  begin
    return A ** N;
  end "**";


  function "*" (X : Scalar; A : Matrix) return Matrix is

    Result : Matrix(A'Range(1), A'Range(2));

  begin
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Result(I, J) := X * A(I, J);
      end loop;
    end loop;
    return Result;
  end "*";


  function "*" (A : Matrix; X : Scalar) return Matrix is

    Result : Matrix(A'Range(1), A'Range(2));

  begin
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Result(I, J) := A(I, J) * X;
      end loop;
    end loop;
    return Result;
  end "*";


  function "/" (A : Matrix; X : Scalar) return Matrix is

    Result : Matrix(A'Range(1), A'Range(2));

  begin
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Result(I, J) := A(I, J) / X;
      end loop;
    end loop;
    return Result;
  end "/";


  function "/" (X : Scalar; A : Matrix) return Matrix is
  begin
    return X * Inverse(A);
  end "/";


  function "*" (A : Matrix; V : Vector) return Vector is

    Result : Vector(A'Range(1)) := (others => Zero);

  begin
    if A'First(2) = V'First and A'Last(2) = V'Last then
      for I in A'Range(1) loop
        for J in A'Range(2) loop
          Result(I) := Result(I) + A(I, J) * V(J);
        end loop;
      end loop;
      return Result;
    else
      raise Bound_Mismatch;
    end if;
  end "*";


  function "*" (V : Vector; A : Matrix) return Vector is

    Result : Vector(A'Range(2)) := (others => Zero);

  begin
    if V'First = A'First(1) and V'Last = A'Last(1) then
      for I in A'Range(1) loop
        for J in A'Range(2) loop
          Result(J) := Result(J) + V(I) * A(I, J);
        end loop;
      end loop;
      return Result;
    else
      raise Bound_Mismatch;
    end if;
  end "*";


  function Identity (First, Last : Index) return Matrix is

     Result : Matrix(First..Last, First..Last);

  begin
    for I in Result'Range(1) loop
      for J in Result'Range(2) loop
        if I = J then
          Result(I, J) := One;
        else
          Result(I, J) := Zero;
        end if;
      end loop;
    end loop;
    return Result;
  end Identity;


  function Transposed (A : Matrix) return Matrix is

    Result : Matrix(A'Range(2), A'Range(1));

  begin
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Result(J, I) := A(I, J);
      end loop;
    end loop;
    return Result;
  end Transposed;


  function Det (A : Matrix) return Scalar is

    M                : Matrix(A'Range(1), A'Range(2)) := A;
    P                : Permutation(A'Range(1));
    Permuted, P_Odd  : Boolean;
    Determinant      : Scalar;

  begin
    LU_Decompose(M, P, P_Odd, Permuted);
    if P_Odd then
      Determinant := -One;
    else
      Determinant := One;
    end if;
    for I in M'Range(1) loop
      Determinant := Determinant * M(I, I);
    end loop;
    return Determinant;
  exception
    when Matrix_Singular =>
      return Zero;
  end Det;


  function Inverse (A : Matrix) return Matrix is
  begin
    return Linear_System_Solution(A, Identity(A'First(1), A'Last(1)));
  end Inverse;


  function Linear_System_Solution (A : Matrix; B : Vector) return Vector is

    LU         : Matrix(A'Range(1), A'Range(2)) := A;
    Perm       : Permutation(A'Range(1));
    Perm_Odd,
    Permuted   : Boolean;

  begin
    LU_Decompose(LU, Perm, Perm_Odd, Permuted);
    if Permuted then
      return LU_System_Solution(LU, Permute(B, Perm));
    else
      return LU_System_Solution(LU, B);
    end if;
  end Linear_System_Solution;


  function Linear_System_Solution (A : Matrix; B : Matrix) return Matrix is

    LU         : Matrix(A'Range(1), A'Range(2)) := A;
    Perm       : Permutation(A'Range(1));
    Perm_Odd,
    Permuted   : Boolean;

    X          : Matrix(B'Range(1), B'Range(2));

  begin
    LU_Decompose(LU, Perm, Perm_Odd, Permuted);
    if Permuted then
      for J in B'Range(2) loop
        Set_Column(X, J, LU_System_Solution(LU, Permute(Column(B, J), Perm)));
      end loop;
    else
      for J in B'Range(2) loop
        Set_Column(X, J, LU_System_Solution(LU, Column(B, J)));
      end loop;
    end if;
    return X;
  end Linear_System_Solution;


  function Permute (V : Vector; P : Permutation) return Vector is

    Result : Vector(V'Range);

  begin
    for I in V'Range loop
      Result(I) := V(P(I));
    end loop;
    return Result;
  end Permute;


  function Row_Permute (A : Matrix; P : Permutation) return Matrix is

    Result : Matrix(A'Range(1), A'Range(2));

  begin
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Result(I, J) := A(P(I), J);
      end loop;
    end loop;
    return Result;
  end Row_Permute;


  function Column_Permute (A : Matrix; P : Permutation) return Matrix is

    Result : Matrix(A'Range(1), A'Range(2));

  begin
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Result(I, J) := A(I, P(J));
      end loop;
    end loop;
    return Result;
  end Column_Permute;


  function Permute (A : Matrix; On_Rows, On_Columns : Permutation) return Matrix is

    Result : Matrix(A'Range(1), A'Range(2));

  begin
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Result(I, J) := A(On_Rows(I), On_Columns(J));
      end loop;
    end loop;
    return Result;
  end Permute;


  procedure Swap is new Exchange(Item => Index);
  procedure Swap is new Exchange(Item => Scalar);


  procedure LU_Decompose (A        : in out Matrix;
                          P        : out Permutation;
                          P_Odd,
                          Permuted : out Boolean) is

    LU_Coeff    : Scalar;
    Perm        : Permutation(A'Range);
    Perm_Odd    : Boolean := False;

  begin
    if not Same_Bounds(A) or A'First(1) /= P'First or A'Last(1) /= P'Last then
      raise Bound_Mismatch;
    end if;
    for I in Perm'Range loop
      Perm(I) := I;
    end loop;
    Permuted := False;
    if A'Last(1) > Index'First then
      for K in A'First(1)..Index'Pred(A'Last(1)) loop
        if A(K, K) = Zero then
          -- zero pivot, try to swap two rows
          for L in Index'Succ(K)..A'Last(1) loop
            if A(L, K) /= Zero then
              for S in A'Range(2) loop
                Swap(A(K, S), A(L, S));
              end loop;
              Swap(Perm(K), Perm(L));
              Permuted := True;
              Perm_Odd := not Perm_Odd;
              exit;
            end if;
          end loop;
          if A(K, K) = Zero then
            -- no nonzero pivot can be found
            raise Matrix_Singular;
          end if;
        end if;
        -- use A(K, K) as the pivot
        for I in Index'Succ(K)..A'Last(1) loop
          LU_Coeff := A(I, K) / A(K, K);
          for J in Index'Succ(K)..A'Last(2) loop
            A(I, J) := A(I, J) - LU_Coeff * A(K, J);
          end loop;
          A(I, K) := LU_Coeff;
        end loop;
      end loop;
    end if;
    P     := Perm;
    P_Odd := Perm_Odd;
  end LU_Decompose;


  function Triangular_System_Solution (A : Matrix; B : Vector) return Vector is

    Aux : Scalar;
    X   : Vector(B'Range);

  begin
    if not Same_Bounds(A) or A'First(1) /= B'First or A'Last(1) /= B'Last then
      raise Bound_Mismatch;
    end if;
    X(B'Last) := B(B'Last) / A(B'Last, B'Last);
    if B'Last > Index'First then
      for I in reverse B'First..Index'Pred(B'Last) loop
        Aux := B(I);
        for J in Index'Succ(I)..B'Last loop
          Aux := Aux - A(I, J) * X(J);
        end loop;
        X(I) := Aux / A(I, I);
      end loop;
    end if;
    return X;
  end Triangular_System_Solution;


  function LU_System_Solution (LU : Matrix; B : Vector) return Vector is

    Aux : Scalar;
    C   : Vector(B'Range) := B;

  begin
    if not Same_Bounds(LU) or LU'First(1) /= B'First or LU'Last(1) /= B'Last then
      raise Bound_Mismatch;
    end if;
    if C'First < Index'Last then
      for K in Index'Succ(C'First)..C'Last loop
        Aux := C(K);
        for J in C'First..Index'Pred(K) loop
          Aux := Aux - LU(K, J) * C(J);
        end loop;
        C(K) := Aux;
      end loop;
    end if;
    return Triangular_System_Solution(LU, C);
  end LU_System_Solution;

end Matrices;
