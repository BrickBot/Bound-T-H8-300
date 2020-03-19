-- GENERIC PACKAGE FOR HANDLING GALOIS FIELDS (GF(p^n))
   ----------------------------------------------------

-- Creation : 22-OCT-1986 by Mats Weber.


with ZpZ_Base_Type,
     Exponentiation_Functions;

package body Galois_Field is
-------------------------

  use ZpZ_Polynomials;


  function To_Element (A : ZpZ) return Element is
  begin
    return A & (1..Element'Last => 0);
  end To_Element;


  function In_ZpZ (A : Element) return Boolean is
  begin
    return A(1..A'Last) = (1..A'Last => 0);
  end;


  function To_ZpZ (A : Element) return ZpZ is
  begin
    if In_ZpZ(A) then
      return A(0);
    else
      raise Not_In_ZpZ;
    end if;
  end To_ZpZ;


  function To_Element (P : Polynomial) return Element is

    Result : Polynomial_N := (others => 0);

  begin
    Result(P'Range) := P;
    return Element(Result);
  end To_Element;


  function To_Polynomial (A : Element) return Polynomial_N is
  begin
    return Polynomial_N(A);
  end To_Polynomial;


  function "+" (A : Element) return Element is
  begin
    return A;
  end "+";


  function "-" (A : Element) return Element is

    B : Element;

  begin
    for I in Element'Range loop
      B(I) := -A(I);
    end loop;
    return B;
  end "-";


  function "+" (A, B : Element) return Element is

    C : Element;

  begin
    for I in Element'Range loop
      C(I) := A(I) + B(I);
    end loop;
    return C;
  end "+";


  function "-" (A, B : Element) return Element is

    C : Element;

  begin
    for I in Element'Range loop
      C(I) := A(I) - B(I);
    end loop;
    return C;
  end "-";


  function "*" (A, B : Element) return Element is
  begin
    return To_Element(Polynomial(A) * Polynomial(B) mod GF_Polynomial);
  end "*";


  function "/" (A, B : Element) return Element is
  begin
    return A * Inverse(B);
  end "/";


  function "**" (A : Element; N : GF_Integer) return Element is

    function "**" is
      new Exponentiation_Functions.Exponentiation(Number   => Element,
                                                  Exponent => GF_Integer,
                                                  One      => One);

  begin
    return A ** N;
  end "**";

  function "**" (A : Element; N : Integer) return Element is

    function "**" is
      new Exponentiation_Functions.Exponentiation(Number   => Element,
                                                  Exponent => Integer,
                                                  One      => One);

  begin
    return A ** N;
  end "**";


  function Inverse (A : Element) return Element is
  begin
    if A = Zero then
      raise Attempt_To_Invert_Zero;
    end if;
    return A ** Integer(Q - 2);
  end Inverse;


  function Order (A : Element) return GF_Positive is

    B : Element := A;
    N : GF_Positive := 1;

  begin
    loop
      if B = One then
        return N;
      end if;
      N := N + 1;
      B := B * A;
    end loop;
  end Order;


  function Minimal_Polynomial (A : Element) return Polynomial is

    Found : exception;

    Result : Polynomial(0..N);


    function Evaluate is
      new ZpZ_Polynomials.Polynomial_Evaluation(Item    => Element,
                                                To_Item => To_Element);

    procedure Return_Polynomial (P : in Polynomial) is
    begin
      if Evaluate(P, X => A) = Zero then
        Assign(Result, P);
        raise Found;
      end if;
    end Return_Polynomial;

    procedure Try_Irreducibles is
      new Enumerate_Irreducibles(Action => Return_Polynomial);

  begin
    for M in 1..N loop
      if N mod M = 0 then
        Try_Irreducibles(Deg => M);
      end if;
    end loop;
  exception
    when Found =>
      return Result(0..Deg(Result));
  end Minimal_Polynomial;


  function Is_Generator (A : Element) return Boolean is
  begin
    return Order(A) = Q - 1;
  end Is_Generator;


  function A_Generator return Element is

    B : Element := Zero;

    use ZpZ_Base_Type;

  begin
    loop
      for I in Element'Range loop
        if B(I) < ZpZ'Last then
          B(I) := ZpZ(ZpZ_Integer(B(I)) + 1);
          exit;
        else
          B(I) := 0;
        end if;
      end loop;
      if Is_Generator(B) then
        return B;
      end if;
    end loop;
  end A_Generator;


  function All_Generators return Element_Array is

    B      : Element := Zero;
    Result : Element_Array (1..Integer(Q - 1));
    N      : Integer range 0..Result'Last := 0;

    use ZpZ_Base_Type;

  begin
    loop
      for I in Element'Range loop
        if B(I) < ZpZ'Last then
          B(I) := ZpZ(ZpZ_Integer(B(I)) + 1);
          exit;
        elsif I = Element'Last then
          return Result(1..N);
        else
          B(I) := 0;
        end if;
      end loop;
      if Is_Generator(B) then
        N := N + 1;
        Result(N) := B;
      end if;
    end loop;
  end All_Generators;


  procedure Enumerate_Elements is

    procedure New_Action (P : in Polynomial) is
    begin
      Action(To_Element(P));
    end;

    procedure Generate_Polynomials is new Enumerate_Polynomials(Action => New_Action);

  begin
    Generate_Polynomials(Max_Deg => Element'Last);
  end Enumerate_Elements;

end Galois_Field;
