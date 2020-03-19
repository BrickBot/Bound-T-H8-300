-- GENERIC PACKAGE FOR HANDLING GALOIS FIELDS (GF(p^n))
   ----------------------------------------------------

-- Revision : 29-JAN-1990 by Mats Weber, optimize "**" using multiplication.
-- Revision : 30-JAN-1986 by Mats Weber, use a table of THE_GENERATOR ** I + ONE for I in 0..Q - 2
--                                       to implement addition.
-- Revision : 27-JAN-1986 by Mats Weber, store only one half of the addition table.

-- Creation : 20-JAN-1987 by Mats Weber.


package body Fast_Galois_Field is
------------------------------

  use Fast_GF_Base_Type;

  use Base_ZpZ_Field,
      Base_ZpZ_Field.ZpZ_Polynomials;


  subtype Nonzero_Element is Element range One..Element'Pred(Zero);


  type To_ZpZ_Converter (Is_In_ZpZ : Boolean := False) is
    record
      case Is_In_ZpZ is
        when True =>
          ZpZ_Value : ZpZ;
        when False =>
          null;
      end case;
    end record;


  Plus_One_Table      : array (Nonzero_Element) of Element;  -- PLUS_ONE_TABLE(A) = A + ONE
  To_Element_Table    : array (ZpZ) of Element;
  To_ZpZ_Table        : array (Element) of To_ZpZ_Converter;

  Negator             : Fast_GF_Integer;


  The_GF_Polynomial   : Polynomial(0..N);
  Generator_Poly      : Polynomial(0..N - 1);


  function The_Generator return Element is
  begin
    return Element'(1);
  end The_Generator;


  function To_Element (A : ZpZ) return Element is
  begin
    return To_Element_Table(A);
  end To_Element;


  function In_ZpZ (A : Element) return Boolean is
  begin
    return To_ZpZ_Table(A).Is_In_ZpZ;
  end;


  function To_ZpZ (A : Element) return ZpZ is
  begin
    if To_ZpZ_Table(A).Is_In_ZpZ then
      return To_ZpZ_Table(A).ZpZ_Value;
    else
      raise Not_In_ZpZ;
    end if;
  end To_ZpZ;


  function GF_Polynomial return Polynomial is
  begin
    return The_GF_Polynomial;
  end;


  function To_Polynomial (A : Element) return Polynomial is
  begin
    if A = Zero then
      return Zero_Polynomial;
    else
      return Generator_Poly(0..Deg(Generator_Poly)) ** Integer(A) mod
             The_GF_Polynomial;
    end if;
  end To_Polynomial;


  function Power_Of_Generator (A : Element) return Power is
  begin
    if A = Zero then
      raise Zero_Is_Not_A_Power;
    else
      return Power(A);
    end if;
  end Power_Of_Generator;


  function "+" (A : Element) return Element is
  begin
    return A;
  end "+";


  function "-" (A : Element) return Element is
  begin
    if A = Zero then
      return Zero;
    end if;
    declare

      C : constant Fast_GF_Integer := Fast_GF_Integer(A) + Negator;

    begin
      if C < Q_1 then
        return Element(C);
      else
        return Element(C - Q_1);
      end if;
    end;
  end "-";


  function "+" (A, B : Element) return Element is
  begin
    if A = Zero then
      return B;
    elsif B = Zero then
      return A;
    elsif A >= B then
      return B * Plus_One_Table(Element(Fast_GF_Integer(A) - Fast_GF_Integer(B)));
    else
      return A * Plus_One_Table(Element(Fast_GF_Integer(B) - Fast_GF_Integer(A)));
    end if;
  end "+";


  function "-" (A, B : Element) return Element is
  begin
    return A + (-B);
  end "-";


  function "*" (A, B : Element) return Element is
  begin
    if A = Zero or B = Zero then
      return Zero;
    end if;
    declare

      C : constant Fast_GF_Integer := Fast_GF_Integer(A) + Fast_GF_Integer(B);

    begin
      if C < Q_1 then
        return Element(C);
      else
        return Element(C - Q_1);
      end if;
    end;
  end "*";


  function "/" (A, B : Element) return Element is
  begin
    if B = Zero then
      raise Attempt_To_Invert_Zero;
    end if;
    if A = Zero then
      return Zero;
    end if;
    declare

      C : constant Fast_GF_Integer := Fast_GF_Integer(A) - Fast_GF_Integer(B);

    begin
      if C >= 0 then
        return Element(C);
      else
        return Element(C + Q_1);
      end if;
    end;
  end "/";


  function "**" (A : Element; N : GF_Integer) return Element is

     N_Reduced : GF_Integer;

  begin
    if A = Zero then
      return Zero;
    end if;
    if abs N <= GF_Integer(Q_1) then
      N_Reduced := N;
    else
      N_Reduced := N mod GF_Integer(Q_1);
    end if;
    return Element(GF_Integer(A) * N_Reduced mod GF_Integer(Q_1));
  end "**";

  function "**" (A : Element; N : Integer) return Element is
  begin
    return A ** GF_Integer(N);
  end "**";


  function Inverse (A : Element) return Element is
  begin
    if A = Zero then
      raise Attempt_To_Invert_Zero;
    end if;
    if A = One then
      return One;
    end if;
    return Element(Q_1 - Fast_GF_Integer(A));
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
    return Order(A) = GF_Integer(Q_1);
  end Is_Generator;


  function A_Generator return Element is
  begin
    return The_Generator;
  end A_Generator;


  function All_Generators return Element_Array is

    Result : Element_Array (1..Integer(Q) - 1);
    N      : Integer range 0..Result'Last := 0;

  begin
    for B in Nonzero_Element loop
      if Is_Generator(B) then
        N := N + 1;
        Result(N) := B;
      end if;
    end loop;
    return Result(1..N);
  end All_Generators;


  procedure Enumerate_Elements is
  begin
    for A in Element loop
      Action(A);
    end loop;
  end Enumerate_Elements;


  procedure Initialize_Tables is separate;

begin
  if P = 2 then
    Negator := 0;
  else
    Negator := Q_1 / 2;  -- Q - 1 is always even in this case
  end if;
  Initialize_Tables;
end Fast_Galois_Field;
