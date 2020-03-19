with TEXT_IO,
     INTEGER_TEXT_IO,
     INTEGER_PRIMES;

use TEXT_IO,
    INTEGER_TEXT_IO,
    INTEGER_PRIMES;

procedure TEST_PRIME_NUMBERS is
----------------------------

  M, N : INTEGER;

begin
  loop
    NEW_LINE;
    PUT("N : ");
    GET(N);
    SKIP_LINE;
    PUT(N, WIDTH => 0);
    if PRIME(N) then
      PUT(" is prime");
    else
      PUT(" is not prime");
    end if;
    NEW_LINE;
    PUT("Smallest divisor of "); PUT(N, WIDTH => 0); PUT(" : ");
    PUT(SMALLEST_DIVISOR(N), WIDTH => 0);
    NEW_LINE;
    declare

      DEC : constant ARRAY_OF_POWER := FACTORIZATION(N);
      DIV : constant INT_ARRAY := DIVISORS(N, INCLUDE_1 => TRUE, INCLUDE_N => TRUE);

    begin
      PUT("Factorization of "); PUT(N, WIDTH => 0); PUT(" : ");
      for I in DEC'RANGE loop
        PUT(DEC(I).BASE, WIDTH => 0);
        if DEC(I).EXPONENT /= 1 then
          PUT("^");
          PUT(DEC(I).EXPONENT, WIDTH => 0);
        end if;
        if I /= DEC'LAST then
          PUT(" * ");
        end if;
      end loop;
      NEW_LINE;
      --
      PUT("Divisors of "); PUT(N, WIDTH => 0); PUT(" :");
      for I in DIV'RANGE loop
        PUT("  ");
        PUT(DIV(I), WIDTH => 0);
      end loop;
      NEW_LINE;
    end;
    exit when N = 0;
  end loop;
  --
  loop
    NEW_LINE;
    PUT("FROM : "); GET(M); SKIP_LINE;
    PUT("TO   : "); GET(N); SKIP_LINE;
    PUT("Prime numbers from ");
    PUT(M, WIDTH => 0);
    PUT(" to ");
    PUT(N, WIDTH => 0);
    PUT(" : ");
    NEW_LINE;
    declare
      P : constant INT_ARRAY := PRIMES(FROM => M, TO => N);
    begin
      for I in P'RANGE loop
        PUT(P(I));
        NEW_LINE;
      end loop;
    end;
    exit when M = 0 and N = 0;
  end loop;
  --
  loop
    NEW_LINE;
    PUT("M : "); GET(M); SKIP_LINE;
    PUT("N : "); GET(N); SKIP_LINE;
    exit when M = 0 and N = 0;
    PUT("GCD(");
    PUT(M, WIDTH => 0);
    PUT(",");
    PUT(N, WIDTH => 0);
    PUT(") = ");
    PUT(GCD(M,N), WIDTH => 0);
    NEW_LINE;
    PUT("LCM(");
    PUT(M, WIDTH => 0);
    PUT(",");
    PUT(N, WIDTH => 0);
    PUT(") = ");
    PUT(LCM(M,N), WIDTH => 0);
    NEW_LINE;
  end loop;
  --
  declare

    NUMBERS : INT_ARRAY(1..100);

    LAST : NATURAL range 0..NUMBERS'LAST;

  begin
    PUT("ENTER A LIST OF NUMBERS, TERMINATE WITH -1");
    NEW_LINE;
    for I in NUMBERS'RANGE loop
      GET(NUMBERS(I));
      SKIP_LINE;
      if NUMBERS(I) = -1 then
        LAST := I - 1;
        exit;
      end if;
    end loop;
    PUT("GCD : "); PUT(GCD(NUMBERS(1..LAST)), WIDTH => 0);
    NEW_LINE;
    PUT("LCM : "); PUT(LCM(NUMBERS(1..LAST)), WIDTH => 0);
    NEW_LINE;
  end;
end TEST_PRIME_NUMBERS;
