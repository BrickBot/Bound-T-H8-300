-- GENERIC PACKAGE FOR HANDLING THE FIELD Z/pZ
   -------------------------------------------

-- Creation :  5-NOV-1986 by Mats Weber.


with Exponentiation_Functions;

package body ZpZ_Field is
----------------------

  use ZpZ_Base_Type;

  ZpZ_Inverses : array (ZpZ range 1..ZpZ'Last) of ZpZ;
    -- array containing the inverses of all elements of Z/pZ.


  function To_ZpZ (N : ZpZ_Integer) return ZpZ is
  begin
    return ZpZ(N mod P);
  end To_ZpZ;


  function "+" (A : ZpZ) return ZpZ is
  begin
    return A;
  end "+";


  function "-" (A : ZpZ) return ZpZ is
  begin
    if A = 0 then
      return 0;
    else
      return ZpZ(P - ZpZ_Integer(A));
    end if;
  end "-";


  function "+" (A, B : ZpZ) return ZpZ is

    C : constant ZpZ_Integer := ZpZ_Integer(A) + ZpZ_Integer(B);

  begin
    if C < P then
      return ZpZ(C);
    else
      return ZpZ(C - P);
    end if;
  end "+";


  function "-" (A, B : ZpZ) return ZpZ is

    C : constant ZpZ_Integer := ZpZ_Integer(A) - ZpZ_Integer(B);

  begin
    if C >= 0 then
      return ZpZ(C);
    else
      return ZpZ(C + P);
    end if;
  end "-";


  function "*" (A, B : ZpZ) return ZpZ is
  begin
    return ZpZ((ZpZ_Integer(A) * ZpZ_Integer(B)) mod P);
  end "*";


  function "/" (A, B : ZpZ) return ZpZ is
  begin
    return A * Inverse(B);
  end "/";


  function "**" (A : ZpZ; N : ZpZ_Integer) return ZpZ is

    function "**" is
      new Exponentiation_Functions.Exponentiation(Number   => ZpZ,
                                                  Exponent => ZpZ_Integer,
                                                  One      => 1);

  begin
    return A ** N;
  end "**";

  function "**" (A : ZpZ; N : Integer) return ZpZ is

    function "**" is
      new Exponentiation_Functions.Exponentiation(Number   => ZpZ,
                                                  Exponent => Integer,
                                                  One      => 1);

  begin
    return A ** N;
  end "**";


  function Inverse (A : ZpZ) return ZpZ is
  begin
    if A = 0 then
      raise Attempt_To_Invert_0;
    end if;
    return ZpZ_Inverses(A);
  end Inverse;


  function Order (A : ZpZ) return ZpZ_Positive is

    B : ZpZ := A;
    N : ZpZ_Positive := 1;

  begin
    loop
      if B = 1 then
        return N;
      end if;
      N := N + 1;
      B := B * A;
    end loop;
  end Order;


  function Is_Generator (A : ZpZ) return Boolean is
  begin
    return Order(A) = P - 1;
  end Is_Generator;


  function A_Generator return ZpZ is
  begin
    for B in ZpZ range 1..ZpZ'Last loop
      if Is_Generator(B) then
        return B;
      end if;
    end loop;
  end A_Generator;


  function All_Generators return ZpZ_Array is

    Result : ZpZ_Array(1..Integer(P) - 1);
    N      : Integer range 0..Result'Last := 0;

  begin
    for A in ZpZ range 1..ZpZ'Last loop
      if Is_Generator(A) then
        N := N + 1;
        Result(N) := A;
      end if;
    end loop;
    return Result(1..N);
  end All_Generators;


  procedure Enumerate_Polynomials (Max_Deg : in ZpZ_Polynomials.Degree) is

    use ZpZ_Polynomials;

    S : Polynomial(0..Max_Deg) := (others => ZpZ'Last);

  begin
    if Max_Deg >= 0 then
      loop
        Action(S);
        for I in S'Range loop
          if S(I) > 0 then
            S(I) := ZpZ(ZpZ_Integer(S(I)) - 1);
            exit;
          elsif I = S'Last then
            return;
          else
            S(I) := ZpZ'Last;
          end if;
        end loop;
      end loop;
    else
      Action(S);  -- in this case, S is a null array.
    end if;
  end Enumerate_Polynomials;


  function Is_Irreducible (P : ZpZ_Polynomials.Polynomial) return Boolean is

    use ZpZ_Polynomials;

    Deg_P : constant Degree := Deg(P);

    Division_Succeeded : exception;

  begin
    if Deg_P <= 1 then
      return True;
    end if;
    declare

      Max_Deg : constant Degree := Deg_P / 2;

    begin
      for M in 1..Max_Deg loop
        declare

          subtype Polynomial_M_1 is Polynomial(0..M - 1);

          procedure Try_Division (Q_Low : in Polynomial_M_1) is
          begin
            if P mod (Q_Low & 1) = Zero_Polynomial then
              raise Division_Succeeded;
            end if;
          end;

          procedure Try_All_Divisions is
            new Enumerate_Polynomials (Action => Try_Division);

        begin
          Try_All_Divisions(Max_Deg => Polynomial_M_1'Last);
        end;
      end loop;
    end;
    return True;    -- no division succeeded
  exception
    when Division_Succeeded =>
      return False;
  end Is_Irreducible;


  function An_Irreducible (Deg : ZpZ_Polynomials.Degree) return ZpZ_Polynomials.Polynomial is

    use ZpZ_Polynomials;

    Result : Polynomial(0..Deg);

    Found : exception;

    procedure Return_Polynomial (P : in Polynomial) is
    begin
      Result := P;
      raise Found;
    end;

    procedure Get_First_Irreducible is
      new Enumerate_Irreducibles (Action => Return_Polynomial);

  begin
    Get_First_Irreducible(Deg);
  exception
    when Found =>
      return Result;
  end An_Irreducible;


  procedure Enumerate_Irreducibles (Deg : in ZpZ_Polynomials.Degree) is

    use ZpZ_Polynomials;

    Q : Polynomial(0..Deg);

    procedure Try_Polynomial (Q_Low : Polynomial) is
    begin
      Q(0..Deg - 1) := Q_Low;
      if Is_Irreducible(Q) then
        Action(Q);
      end if;
    end;

    procedure Try_All_Polynomials is
      new Enumerate_Polynomials (Action => Try_Polynomial);

  begin
    if Deg >= 0 then
      Q(Deg) := 1;
      Try_All_Polynomials(Max_Deg => Deg - 1);
    else
      Action(Q);  -- in this case, Q is a null array.
    end if;
  end Enumerate_Irreducibles;


  procedure Check_Instantiation is separate;
    -- Checks if P is prime and raises ZPZ_FIELD_EXCEPTIONS.P_NOT_PRIME if not.


begin
  Check_Instantiation;
  for A in ZpZ range 1..ZpZ'Last loop
    ZpZ_Inverses(A) := A ** (P - 2);
  end loop;
end ZpZ_Field;
