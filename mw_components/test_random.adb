with RANDOM_NUMERIC_TYPES,
     RANDOM,
     TEXT_IO,
     INTEGER_TEXT_IO;

use RANDOM_NUMERIC_TYPES,
    RANDOM,
    TEXT_IO,
    INTEGER_TEXT_IO;

procedure TEST_RANDOM is

  NUMBER, LOWER, UPPER : INTEGER;

  procedure INCR(J : in out INTEGER) is
  begin
    J := J+1;
  end INCR;

  package RANDOM_REAL_TEXT_IO is new FLOAT_IO(RANDOM_REAL);
  use RANDOM_REAL_TEXT_IO;

begin
  RANDOMIZE;
  for I in 1..100 loop
    PUT(NORMAL, AFT => 5, EXP => 0);
    NEW_LINE;
  end loop;
  PUT("Number of tests : ");
  GET(NUMBER);
  PUT("Lower bound : ");
  GET(LOWER);
  PUT("Upper bound : ");
  GET(UPPER);
  RESET;
  declare
    FREQ : array (LOWER..UPPER) of NATURAL := (LOWER..UPPER => 0);
  begin
    for I in 1..NUMBER loop
      INCR(FREQ(INTEGER(UNIFORM(RANDOM_INTEGER(LOWER), RANDOM_INTEGER(UPPER)))));
    end loop;
    PUT_LINE("RELATIVE FREQUENCIES :");
    for I in LOWER..UPPER loop
      PUT(I);
      PUT(FREQ(I));
      NEW_LINE;
    end loop;
  end;
  --
  for ZZ in 1..2 loop
    declare
      LOW : constant := -40;
      UP : constant := 40;
      MAX : NATURAL := 0;
      FREQ : array (LOW..UP) of NATURAL := (others => 0);
    begin
      case ZZ is
        when 1 =>
          for J in 1..NUMBER loop
            INCR(FREQ(INTEGER(RANDOM_REAL(UP+LOW)/2.0 + RANDOM_REAL(UP-LOW)/12.0 * NORMAL)));
          end loop;
        when 2 =>
          for J in 1..NUMBER loop
            INCR(FREQ(INTEGER(RANDOM_REAL(UP+LOW)/2.0 + RANDOM_REAL(UP-LOW)/20.0 * EXPONENTIAL)));
          end loop;
      end case;
      for I in LOW..UP loop
        if FREQ(I) > MAX then
          MAX := FREQ(I);
        end if;
      end loop;
      for I in LOW..UP loop
        for J in 1..79*FREQ(I)/MAX loop
          PUT('*');
        end loop;
        NEW_LINE;
      end loop;
    end;
  end loop;
end TEST_RANDOM;
