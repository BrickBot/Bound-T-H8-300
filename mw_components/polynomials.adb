-- GENERIC PACKAGE FOR POLYNOMIALS
   -------------------------------

-- Creation : 21-OCT-1986 by Mats Weber.


with Min_Max_Functions;

package body Polynomials is
------------------------

  function Max is new Min_Max_Functions.Maximum(Index);
  function Min is new Min_Max_Functions.Minimum(Index);


  function Equal (P, Q : Polynomial) return Boolean is

    Deg_P : Degree := Deg(P);

  begin
    return Deg_P = Deg(Q) and then P(0..Deg_P) = Q(0..Deg_P);
  end Equal;


  procedure Assign (Object : out Polynomial; Value : in Polynomial) is
  begin
    Object := (others => Zero);
    Object(Value'Range) := Value;
  end Assign;


  function "+" (P : Polynomial) return Polynomial is

    Result : Polynomial(0..P'Last);

  begin
    for I in Result'Range loop
      Result(I) := +P(I);
    end loop;
    return Result(0..Deg(Result));
  end "+";


  function "-" (P : Polynomial) return Polynomial is

    Result : Polynomial(0..P'Last);

  begin
    for I in Result'Range loop
      Result(I) := -P(I);
    end loop;
    return Result(0..Deg(Result));
  end "-";


  function "+" (P, Q : Polynomial) return Polynomial is

    Result     : Polynomial(0..Max(P'Last, Q'Last));
    Min_Degree : Degree;

  begin
    if P'Last < Q'Last then
      Min_Degree := P'Last;
      Result(P'Last + 1..Result'Last) := Q(P'Last + 1..Q'Last);
    elsif P'Last = Q'Last then
      Min_Degree := P'Last;
    else
      Min_Degree := Q'Last;
      Result(Q'Last + 1..Result'Last) := P(Q'Last + 1..P'Last);
    end if;
    for I in 0..Min_Degree loop
      Result(I) := P(I) + Q(I);
    end loop;
    return Result(0..Deg(Result));
  end "+";


  function "-" (P, Q : Polynomial) return Polynomial is
  begin
    return P + (-Q);
  end "-";


  function "*" (P, Q : Polynomial) return Polynomial is

    Result : Polynomial(0..P'Last + Q'Last) := (others => Zero);

  begin
    for K in Result'Range loop
      for I in Max(0, K - Q'Last)..Min(P'Last, K) loop
        Result(K) := Result(K) + P(I) * Q(K - I);
      end loop;
    end loop;
    return Result(0..Deg(Result));
  end "*";


  function "/" (P, Q : Polynomial) return Polynomial is

    Deg_P : constant Degree := Deg(P);
    Deg_Q : constant Degree := Deg(Q);

    Quotient  : Polynomial(0..Deg_P - Deg_Q) := (others => Zero);
    Remainder : Polynomial(0..Deg_P) := P(0..Deg(P));

  begin
    for N in reverse Quotient'Range loop
      Quotient(N) := Remainder(N + Deg_Q) / Q(Deg_Q);
      for I in 0..Deg_Q loop
        Remainder(N + I) := Remainder(N + I) - Quotient(N) * Q(I);
      end loop;
    end loop;
    return Quotient;
  end "/";


  function "mod" (P, Q : Polynomial) return Polynomial is

    Deg_P : constant Degree := Deg(P);
    Deg_Q : constant Degree := Deg(Q);

    Quotient_N : Coefficient;
    Remainder  : Polynomial(0..Deg_P) := P(0..Deg_P);

  begin
    for N in reverse 0..Deg_P - Deg_Q loop
      Quotient_N := Remainder(N + Deg_Q) / Q(Deg_Q);
      for I in 0..Deg_Q loop
        Remainder(N + I) := Remainder(N + I) - Quotient_N * Q(I);
      end loop;
    end loop;
    return Remainder(0..Deg(Remainder));
  end "mod";


  function "*" (X : Coefficient; P : Polynomial) return Polynomial is

    Result : Polynomial(0..P'Last);

  begin
    for I in Result'Range loop
      Result(I) := X * P(I);
    end loop;
    return Result(0..Deg(Result));
  end "*";


  function "*" (P : Polynomial; X : Coefficient) return Polynomial is

    Result : Polynomial(0..P'Last);

  begin
    for I in Result'Range loop
      Result(I) := P(I) * X;
    end loop;
    return Result(0..Deg(Result));
  end "*";


  function "/" (P : Polynomial; X : Coefficient) return Polynomial is

    Result : Polynomial(0..P'Last);

  begin
    for I in Result'Range loop
      Result(I) := P(I) / X;
    end loop;
    return Result(0..Deg(Result));
  end "/";


  function "**" (P : Polynomial; N : Natural) return Polynomial is

    subtype Polynomial_P_N is Polynomial(0..Index(N) * Deg(P));

    Result,
    P_Raised : Polynomial_P_N;
    M        : Natural := N;

  begin
    Result(0) := One;
    Result(1..Result'Last) := (others => Zero);
    Assign(P_Raised, P);
    loop
      if M mod 2 /= 0 then
        Assign(Result, Result * P_Raised);
      end if;
      M := M / 2;
      exit when M = 0;
      Assign(P_Raised, P_Raised * P_Raised);
    end loop;
    return Result(0..Deg(Result));
  end "**";


  function Deg (P : Polynomial) return Degree is
  begin
    for I in reverse P'Range loop
      if P(I) /= Zero then
        return I;
      end if;
    end loop;
    return -1;
  end Deg;


  function Polynomial_Evaluation (P : Polynomial; X : Item) return Item is

    Result : Item := To_Item(P(P'Last));

  begin
    for I in reverse P'First..P'Last - 1 loop
      Result := Result * X + To_Item(P(I));
    end loop;
    return Result;
  end Polynomial_Evaluation;

end Polynomials;
