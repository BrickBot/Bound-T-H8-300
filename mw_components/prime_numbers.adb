-- GENERIC PACKAGE FOR PRIME NUMBERS AND RELATED OPERATIONS
   --------------------------------------------------------

-- Revision :  4-NOV-1986 by Mats Weber, use INTEGER_SQRT from MATH_PACK.

-- Creation : 27-OCT-1986 by Mats Weber.


with Queues;

package body Prime_Numbers is
--------------------------

  Two   : constant Int := One + One;
  Three : constant Int := Two + One;


  function Smallest_Divisor (N : Int) return Int is

    M : constant Int := abs N;

  begin
    if M <= One then
      return One;
    end if;
    if Equal(M mod Two, Zero) then
      return Two;
    else
      declare

        Divisor : Int := Three;
        Last    : constant Int := Sqrt(M);

      begin
        while Divisor <= Last loop
          if Equal(M mod Divisor, Zero) then
            return Divisor;
          end if;
          Divisor := Divisor + Two;
        end loop;
        return M;
      end;
    end if;
  end Smallest_Divisor;


  function Prime (N : Int) return Boolean is
  begin
    if N < Zero then
      raise Negative_Number;
    end if;
    if Equal(N, One) then
      return False;
    end if;
    if Equal(N mod Two, Zero) then
      return Equal(N, Two);
    else
      declare

        Divisor : Int := Three;
        Last    : constant Int := Sqrt(N);

      begin
        while Divisor <= Last loop
          if Equal(N mod Divisor, Zero) then
            return Equal(N, Three);
          end if;
          Divisor := Divisor + Two;
        end loop;
        return True;
      end;
    end if;
  end Prime;


  function Primes (From, To : Int) return Int_Array is

    First, Last : Int;

  begin
    if From < Zero or To < Zero then
      raise Negative_Number;
    end if;
    if Equal(From mod Two, Zero) then
      First := From + One;
    else
      First := From;
    end if;
    if Equal(First, One) then
      First := Three;
    end if;
    if Equal(To, Zero) then
      Last := One;
    elsif Equal(To mod Two, Zero) then
      Last := To - One;
    else
      Last := To;
    end if;
    declare

      Sieve    : Int_Array(1..To_Integer((Last - First) / Two) + 1);
      N_Primes : Natural := 0;

      Divisor  : Int := Three;
      Last_Div : constant Int := Sqrt(Last);

    begin
      declare

        K : Int := First;

      begin
        for I in Sieve'Range loop
          Sieve(I) := K;
          exit when I = Sieve'Last;
          K := K + Two;
        end loop;
      end;
      while Divisor <= Last_Div loop
        if Prime(Divisor) then
          for I in Sieve'Range loop
            if not Equal(Sieve(I), Zero) and then
               Equal(Sieve(I) mod Divisor, Zero) and then
               not Equal(Sieve(I), Divisor)
            then
              Sieve(I) := Zero;
            end if;
          end loop;
        end if;
        Divisor := Divisor + Two;
      end loop;
      for I in Sieve'Range loop
        if not Equal(Sieve(I), Zero) then
          N_Primes := N_Primes + 1;
        end if;
      end loop;
      if From <= Two and Two <= To then
        N_Primes := N_Primes + 1;
      end if;
      declare

        Result : Int_Array(1..N_Primes);
        Index  : Integer range 0..N_Primes := 0;

      begin
        if From <= Two and Two <= To then
          Index := Index + 1;
          Result(Index) := Two;
        end if;
        for I in Sieve'Range loop
          if not Equal(Sieve(I), Zero) then
            Index := Index + 1;
            Result(Index) := Sieve(I);
          end if;
        end loop;
        return Result;
      end;
    end;
  end Primes;


  function Factorization (N : Int) return Array_Of_Power is

    R : Int := N;
    P : Power;

    package Queues_Of_Power is new Queues(Item_Type => Power,
                                          Count     => Natural);

    Powers : Queues_Of_Power.Queue;

    use Queues_Of_Power;

  begin
    if R < Zero then
      Put(Item => (Base => -One, Exponent => 1), Into => Powers);
      R := -R;
    end if;
    if Equal(R, Zero) then
      Put(Item => (Base => Zero, Exponent => 1), Into => Powers);
    else
      loop
        P.Base := Smallest_Divisor(R);
        exit when Equal(P.Base, One);
        P.Exponent := 1;
        loop
          R := R / P.Base;
          exit when not Equal(R mod P.Base, Zero);
          P.Exponent := P.Exponent + 1;
        end loop;
        Put(P, Into => Powers);
      end loop;
    end if;
    declare

      Result : Array_Of_Power(1..Length(Powers));

    begin
      for I in Result'Range loop
        Get(Item => Result(I), From => Powers);
      end loop;
      return Result;
    end;
  end Factorization;


  function Divisors (N : Int; Include_1, Include_N : Boolean := False) return Int_Array is

    M      : constant Int := abs N;
    Sqrt_M : constant Int := Sqrt(M);

    package Int_Queues is new Queues(Item_Type => Int,
                                     Count     => Natural);

    Divisors_Of_M : Int_Queues.Queue;
    N_Divisors    : Natural;

    use Int_Queues;

  begin
    if M <= One then
      if Include_1 or Include_N then
        return (1 => One);
      else
        return (1..0 => One);
      end if;
    end if;
    declare

      D : Int := Two;

    begin
      while D <= Sqrt_M loop
        if Equal(M mod D, Zero) then
          Put(Item => D, Into => Divisors_Of_M);
        end if;
        D := D + One;
      end loop;
    end;
    if Equal(Sqrt_M * Sqrt_M, M) then
      N_Divisors := 2 * Length(Divisors_Of_M) - 1;
    else
      N_Divisors := 2 * Length(Divisors_Of_M);
    end if;
    declare

      Result : Int_Array(1..N_Divisors);

    begin
      for I in 1..Length(Divisors_Of_M) loop
        Get(Item => Result(I), From => Divisors_Of_M);
        Result(Result'Last - I + 1) := M / Result(I);
      end loop;
      if Include_1 and Include_N then
        return One & Result & M;
      elsif Include_1 then
        return One & Result;
      elsif Include_N then
        return Result & M;
      else
        return Result;
      end if;
    end;
  end Divisors;


  function GCD (M, N : Int) return Int is

    Dividend  : Int := abs M;
    Divisor   : Int := abs N;
    Remainder : Int;

  begin
    if Equal(N, Zero) and not Equal(M, Zero) then
      return M;
    end if;
    loop
      Remainder := Dividend mod Divisor;
      exit when Equal(Remainder, Zero);
      Dividend := Divisor;
      Divisor  := Remainder;
    end loop;
    return Divisor;
  end GCD;


  function GCD (Numbers : Int_Array) return Int is

    The_GCD : Int := Numbers(Numbers'First);

  begin
    for I in Numbers'First + 1..Numbers'Last loop
      The_GCD := GCD(The_GCD, Numbers(I));
    end loop;
    return The_GCD;
  end GCD;


  function LCM (M, N : Int) return Int is
  begin
    return (abs M / GCD(M, N)) * abs N;
  end LCM;


  function LCM (Numbers : Int_Array) return Int is

    The_LCM : Int := Numbers(Numbers'First);

  begin
    for I in Numbers'First + 1..Numbers'Last loop
      The_LCM := LCM(The_LCM, Numbers(I));
    end loop;
    return The_LCM;
  end LCM;


  function Euler (N : Int) return Int is

    Count : Int := One;
    I     : Int := Two;

  begin
    while I < N loop
      if Equal(GCD(N, I), One) then
        Count := Count + One;
      end if;
      I := I + One;
    end loop;
    return Count;
  end Euler;

end Prime_Numbers;
