-- GENERIC PACKAGE FOR THE STUDY OF BCH CODES
   ------------------------------------------

-- Revision : 20-JAN-1987 by Mats Weber, avoided to keep the base of the code's
--                                       vector space in memory.

-- Creation :  6-NOV-1986 by Mats Weber.


with Canonical_BCH_Codes,
     BCH_Code_Exceptions;

package body BCH_Code is
---------------------

  BCH_Polynomial         : Word_Polynomial;
  Deg_BCH_Polynomial     : Integer range -1..Word_Polynomial'Last;


  Distribution_Computed  : Boolean := False;

  The_Distribution       : Distribution;
  The_Hamming_Number     : Hamming_Weight;


  function In_Code (W : Word) return Boolean is
  begin
    return Polynomial(W) mod BCH_Polynomial(0..Deg_BCH_Polynomial) = Zero_Polynomial;
  end;


  function Dimension return Positive is
  begin
    return Integer(Q) - 1 - Deg_BCH_Polynomial;
  end;


  function Base return Array_Of_Word is

    Code_Base : Array_Of_Word(1..Dimension) := (others =>
                                                  (others => 0));

    G : Polynomial renames BCH_Polynomial(0..Deg_BCH_Polynomial);

  begin
    for I in Code_Base'Range loop
      for J in G'Range loop
        Code_Base(I)(I - 1 + J) := G(J);
      end loop;
    end loop;
    return Code_Base;
  end Base;


  function Weight (W : Word) return Hamming_Weight is

    N : Hamming_Weight := 0;

  begin
    for I in Word'Range loop
      if W(I) /= 0 then
        N := N + 1;
      end if;
    end loop;
    return N;
  end Weight;


  function Distance (W1, W2 : Word) return Hamming_Weight is

    N : Hamming_Weight := 0;

  begin
    for I in Word'Range loop
      if W1(I) /= W2(I) then
        N := N + 1;
      end if;
    end loop;
    return N;
  end Distance;


  function To_Word (A : Polynomial) return Word is

    The_Word : Word_Polynomial := (others => 0);

  begin
    The_Word(A'Range) := A;
    return Word(The_Word);
  end To_Word;


  function "+" (W1, W2 : Word) return Word is

    Result : Word;

  begin
    for I in Word'Range loop
      Result(I) := W1(I) + W2(I);
    end loop;
    return Result;
  end "+";


  function "-" (W1, W2 : Word) return Word is

    Result : Word;

  begin
    for I in Word'Range loop
      Result(I) := W1(I) - W2(I);
    end loop;
    return Result;
  end "-";


  function "*" (A : ZpZ; W : Word) return Word is

    Result : Word;

  begin
    for I in Word'Range loop
      Result(I) := A * W(I);
    end loop;
    return Result;
  end "*";


  function "*" (W : Word; A : ZpZ) return Word is

    Result : Word;

  begin
    for I in Word'Range loop
      Result(I) := W(I) * A;
    end loop;
    return Result;
  end "*";


  function "/" (W : Word; A : ZpZ) return Word is

    Result : Word;

  begin
    for I in Word'Range loop
      Result(I) := W(I) / A;
    end loop;
    return Result;
  end "/";


  function Generator_Polynomial return Polynomial is
  begin
    return BCH_Polynomial(0..Deg_BCH_Polynomial);
  end;


  function Associated_Polynomial return Polynomial is

    X_Q_1_1 : constant Polynomial := -1 & (1..Integer(Q) - 2 => 0) & 1;
      -- x^(q-1) - 1

  begin
    return X_Q_1_1 / BCH_Polynomial(0..Deg_BCH_Polynomial);
  end Associated_Polynomial;


  generic
    with procedure Action (W : in Word);
  procedure Word_Enumeration;

  procedure Word_Enumeration is

    subtype Index is Integer range 1..Dimension;

    G : Polynomial renames BCH_Polynomial(0..Deg_BCH_Polynomial);

    Code_Word : Word := (others => 0);


    procedure Try_Word (N : in Index) is
    begin
      for A in ZpZ loop
        if N = Index'Last then
          Action(Code_Word);
        else
          Try_Word(N => N + 1);
        end if;
        for J in G'Range loop
          Code_Word(N - 1 + J) := Code_Word(N - 1 + J) + G(J);
        end loop;
      end loop;
    end Try_Word;

  begin
    Try_Word(N => 1);
  end Word_Enumeration;


  procedure Compute_Distribution is

    procedure Accumulate (W : in Word) is

      Code_Word_Weight : constant Hamming_Weight := Weight(W);

    begin
      The_Distribution(Code_Word_Weight) := The_Distribution(Code_Word_Weight) + 1;
    end Accumulate;


    procedure Try_All_Words is new Word_Enumeration(Action => Accumulate);

  begin
    The_Distribution := (others => 0);
    Try_All_Words;
    for I in 1..Distribution'Last loop
      if The_Distribution(I) /= 0 then
        The_Hamming_Number := I;
        exit;
      end if;
    end loop;
    Distribution_Computed := True;
  end Compute_Distribution;


  function Hamming_Number return Hamming_Weight is
  begin
    if not Distribution_Computed then
      Compute_Distribution;
    end if;
    return The_Hamming_Number;
  end Hamming_Number;


  function Errors_Corrected return Natural is
  begin
    return (Hamming_Number - 1) / 2;
  end;


  function Errors_Detected return Natural is
  begin
    return Hamming_Number - 1;
  end;


  function Weight_Distribution return Distribution is
  begin
    if not Distribution_Computed then
      Compute_Distribution;
    end if;
    return The_Distribution;
  end Weight_Distribution;


  function Approximative_Weight_Distribution (N_Words : Natural) return Distribution is

    G                : Polynomial renames BCH_Polynomial(0..Deg_BCH_Polynomial);

    Approx_Dis       : Distribution := (others => 0);

    Code_Word        : Word;
    Code_Word_Weight : Hamming_Weight;

  begin
    for M in 1..N_Words loop
      Code_Word := (others => 0);
      for I in 0..Dimension - 1 loop
        declare

          A : constant ZpZ := Uniform;

        begin
          for J in G'Range loop
            Code_Word(I + J) := Code_Word(I + J) + A * G(J);
          end loop;
        end;
      end loop;
      Code_Word_Weight := Weight(Code_Word);
      Approx_Dis(Code_Word_Weight) := Approx_Dis(Code_Word_Weight) + 1;
    end loop;
    return Approx_Dis;
  end Approximative_Weight_Distribution;


  function Canonical_Representation return Boolean is
  begin
    return Canonical_BCH_Codes.Canonical(P, N, T);
  end;


  function BCH_Bound return Positive is
  begin
    return Canonical_BCH_Codes.BCH_Bound(P, N, T);
  end;


begin
  declare

    use ZpZ_Base_Type,
        GF_Base_Type;

  begin
    if not (Q = GF_Positive(P) ** N and
            T in 1..Integer(Q) - 1 and
            ZpZ_Integer(ZpZ'First) = 0 and
            ZpZ_Integer(ZpZ'Last) = P - 1)
    then
      raise BCH_Code_Exceptions.Inconsistent_Parameters;
    end if;
  end;
  declare

    Power_Of_Generator : Element := One;

  begin
    Assign(BCH_Polynomial, Value => (0 => 1));
    for I in 1..T - 1 loop
      Power_Of_Generator := Generator * Power_Of_Generator;
      if Canonical_BCH_Codes.Canonical(P, N, I) then
        Assign(BCH_Polynomial,
               Value => BCH_Polynomial * Minimal_Polynomial(Power_Of_Generator));
      end if;
    end loop;
    Deg_BCH_Polynomial := Deg(BCH_Polynomial);
  end;
end BCH_Code;
