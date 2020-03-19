with TRUNCATION_FUNCTIONS,
     EXPONENTIATION_FUNCTIONS,
     SQUARE_ROOT_FUNCTIONS,
     GENERIC_ELEMENTARY_FUNCTIONS,
     TEXT_IO,
     FLOAT_TEXT_IO,
     INTEGER_TEXT_IO;

use TEXT_IO,
    FLOAT_TEXT_IO,
    INTEGER_TEXT_IO;

procedure TEST_MATH_PACK is

  R : FLOAT;

  function FLOOR   is new TRUNCATION_FUNCTIONS.FLOAT_FLOOR(FLOAT,INTEGER);
  function CEILING is new TRUNCATION_FUNCTIONS.FLOAT_CEILING(FLOAT,INTEGER);

  function FLOOR   is new TRUNCATION_FUNCTIONS.FLOAT_FLOOR(LONG_FLOAT,INTEGER);

begin
  INTEGER_TEXT_IO.DEFAULT_WIDTH := 0;
  FLOAT_TEXT_IO.DEFAULT_FORE := 0;
  FLOAT_TEXT_IO.DEFAULT_AFT := 3;
  FLOAT_TEXT_IO.DEFAULT_EXP := 0;
  --
  goto CONTINUE;
  for I in INTEGER range -100..100 loop
    R := FLOAT(I)/10.0;
    PUT("FLOOR("); PUT(R); PUT(") = "); PUT(FLOOR(R));
    SET_COL(30);
    PUT("CEILING("); PUT(R); PUT(") = "); PUT(CEILING(R));
    NEW_LINE;
  end loop;
<< CONTINUE >>
  --
  declare

    A : FLOAT;
    N : INTEGER;

    function INVERSE (X : FLOAT) return FLOAT is
    begin
      return 1.0/X;
    end;

    function "**" is
      new EXPONENTIATION_FUNCTIONS.EXPONENTIATION(NUMBER   => FLOAT,
                                                  EXPONENT => INTEGER,
                                                  ONE      => 1.0 );

  begin
    loop
      PUT("ENTER A : "); GET(A); SKIP_LINE;
      PUT("ENTER N : "); GET(N); SKIP_LINE;
      PUT("A**N = "); PUT(A**N); NEW_LINE;
      exit when A = 0.0 and N = 0;
    end loop;
  end;
  --
  declare

    package LONG_FLOAT_MATH_LIB is new GENERIC_ELEMENTARY_FUNCTIONS(LONG_FLOAT);

    N : INTEGER;

    function SQRT is new SQUARE_ROOT_FUNCTIONS.INTEGER_SQRT(INTEGER);

  begin
    for K in 0..1000 loop
      if SQRT(K) /= FLOOR(LONG_FLOAT_MATH_LIB.SQRT(LONG_FLOAT(K))) then
        PUT_LINE("SQUARE ROOT DOES NOT WORK");
      end if;
    end loop;
    loop
      PUT("N : ");
      GET(N); SKIP_LINE;
      PUT("SQRT("); PUT(N, WIDTH => 0); PUT(") = ");
      PUT(SQRT(N), WIDTH => 0); NEW_LINE;
      exit when N = 0;
    end loop;
  end;
  --
  declare

    type FIX is delta 0.001 range -100.0..100.0;

    package FIX_TEXT_IO is new TEXT_IO.FIXED_IO(FIX);
    use FIX_TEXT_IO;

    N : FIX;

    function SQRT is new SQUARE_ROOT_FUNCTIONS.FIXED_SQRT(FIX);

  begin
    -- for K in 0..100000 loop
      -- N := K*0.001;
      -- PUT("SQRT("); PUT(N, FORE => 0); PUT(") = ");
      -- PUT(SQRT(N), FORE => 0); NEW_LINE;
    -- end loop;
    loop
      PUT("N : ");
      GET(N); SKIP_LINE;
      PUT("SQRT("); PUT(N, FORE => 0); PUT(") = ");
      PUT(SQRT(N), FORE => 0);
      PUT("    ");
      PUT(SQRT(N), FORE => 0); PUT("**2 = ");
      PUT(FIX(SQRT(N)*SQRT(N)), FORE => 0); NEW_LINE;
      exit when N = 0.0;
    end loop;
  end;
end TEST_MATH_PACK;
