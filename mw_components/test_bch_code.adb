with ZPZ_BASE_TYPE,
     GALOIS_FIELD,
     BCH_CODE,
     MIN_MAX_FUNCTIONS,
     RANDOM,
     TEXT_IO,
     INTEGER_TEXT_IO;

use ZPZ_BASE_TYPE,
    TEXT_IO,
    INTEGER_TEXT_IO;

procedure TEST_BCH_CODE is
-----------------------

  function GET_INTEGER (PROMPT : STRING) return INTEGER is

    N : INTEGER;

  begin
    PUT(PROMPT);
    GET(N);
    SKIP_LINE;
    return N;
  end;


  function YES_NO_ANSWER (QUESTION : STRING) return BOOLEAN is
    CH : CHARACTER;
  begin
    loop
      PUT(QUESTION);
      loop
        GET(CH);
        exit when CH /= ' ';
      end loop;
      SKIP_LINE;
      exit when CH = 'Y' or CH = 'N' or CH = 'y' or CH = 'n';
    end loop;
    return CH = 'Y' or CH = 'y';
  end YES_NO_ANSWER;

begin
  begin
    SET_LINE_LENGTH(132);
  exception
    when USE_ERROR => null;  -- VAX Ada raises this exception if the file
  end;                       -- is associated with a terminal.
  declare

    P : constant ZPZ_POSITIVE := ZPZ_POSITIVE(GET_INTEGER("Enter p : "));
    N : constant POSITIVE     := GET_INTEGER("Enter n : ");
    T : constant POSITIVE     := GET_INTEGER("Enter t : ");

    package GF_P_N is new GALOIS_FIELD(P,N);

    use GF_P_N,
        GF_P_N.BASE_ZPZ_FIELD,
        GF_P_N.BASE_ZPZ_FIELD.ZPZ_POLYNOMIALS;

    package BCH_CODE_P_N_T is new BCH_CODE(P => P,
                                           N => N,
                                           Q => Q,
                                           T => T,
                                           ZPZ => ZPZ,
                                           ELEMENT => ELEMENT,
                                           ZERO => ZERO,
                                           ONE  => ONE,
                                           GENERATOR => A_GENERATOR,
                                           POLYNOMIAL => POLYNOMIAL,
                                           ZERO_POLYNOMIAL => ZERO_POLYNOMIAL);

    use BCH_CODE_P_N_T;

    package ZPZ_TEXT_IO is new INTEGER_IO(ZPZ);
    use ZPZ_TEXT_IO;


    function GET_POLYNOMIAL (PROMPT : STRING) return POLYNOMIAL is

      DEG : DEGREE;

    begin
      PUT_LINE(PROMPT);
      PUT("Degree : "); GET(DEG); SKIP_LINE;
      declare
        P : POLYNOMIAL(0..DEG);
      begin
        for I in reverse P'RANGE loop
          PUT("Coefficient of x^"); PUT(I, WIDTH => 0); PUT(" : ");
          GET(P(I)); SKIP_LINE;
        end loop;
        return P;
      end;
    end GET_POLYNOMIAL;


    procedure PUT (P : in POLYNOMIAL) is

      DEG_P : constant DEGREE := DEG(P);

    begin
      if DEG_P = -1 then
        PUT("0");
      else
        for I in reverse P'RANGE loop
          if P(I) /= 0 then
            if P(I) >= 0 then
              if I /= DEG_P then
                PUT(" + ");
              end if;
            else
              if I = DEG_P then
                PUT("- ");
              else
                PUT(" - ");
              end if;
            end if;
            if abs P(I) /= 1 or I = 0 then
              PUT(abs P(I), WIDTH => 0);
            end if;
            case I is
              when 0 =>
                null;
              when 1 =>
                PUT("x");
              when others =>
                PUT("x^"); PUT(I, WIDTH => 0);
            end case;
          end if;
        end loop;
      end if;
    end PUT;


    procedure PUT_LINE (P : in POLYNOMIAL) is
    begin
      PUT(P);
      NEW_LINE;
    end;

  begin
    PUT("This representation is ");
    if not CANONICAL_REPRESENTATION then
      PUT("not ");
    end if;
    PUT("canonical");
    NEW_LINE;
    PUT("The canonical representation for this code is BCH(");
    PUT(INTEGER(P), WIDTH => 0); PUT(',');
    PUT(N, WIDTH => 0); PUT(',');
    PUT(BCH_BOUND, WIDTH => 0); PUT(").");
    NEW_LINE;
    PUT("Generator : ");
    PUT(TO_POLYNOMIAL(GF_GENERATOR));
    NEW_LINE;
    PUT("Generator polynomial g(x) : ");
    PUT(GENERATOR_POLYNOMIAL);
    NEW_LINE;
    PUT("Weight of the generator polynomial : ");
    PUT(WEIGHT(TO_WORD(GENERATOR_POLYNOMIAL)), WIDTH => 0);
    NEW_LINE;
    PUT("Associated polynomial h(x) : ");
    PUT(ASSOCIATED_POLYNOMIAL);
    NEW_LINE;
    PUT("g(x) * h(x) : ");
    PUT(GENERATOR_POLYNOMIAL * ASSOCIATED_POLYNOMIAL);
    NEW_LINE;
    PUT("Dimension of the code : ");
    PUT(DIMENSION, WIDTH => 0);
    NEW_LINE;
    while YES_NO_ANSWER("Display weight distribution ? (y/n) ") loop
      NEW_LINE;
      declare

        MAX_BAR_LENGTH : constant := 100;

        type NATURAL_ARRAY is array (HAMMING_WEIGHT range <>) of NATURAL;

        function MAX is new MIN_MAX_FUNCTIONS.ARRAY_MAXIMUM(INDEX      => HAMMING_WEIGHT,
                                                            ITEM       => NATURAL,
                                                            ITEM_ARRAY => NATURAL_ARRAY);

        FREQUENCIES   : DISTRIBUTION;
        MAX_FREQUENCY : NATURAL;
        COEFFICIENT   : FLOAT;

        EXACT : constant BOOLEAN := YES_NO_ANSWER("Exact distribution ? (y/n) ");

        function UNIFORM is new RANDOM.ENUMERATION_UNIFORM(ZPZ);

        function APPROXIMATIVE_DISTRIBUTION is
          new APPROXIMATIVE_WEIGHT_DISTRIBUTION(UNIFORM);

      begin
        if EXACT then
          FREQUENCIES := WEIGHT_DISTRIBUTION;
        else
          declare
            N : NATURAL;
          begin
            PUT("How many random words ? ");
            GET(N);
            SKIP_LINE;
            FREQUENCIES := APPROXIMATIVE_DISTRIBUTION(N);
          end;
        end if;
        PUT_LINE("Weight distribution : ");
        NEW_LINE;
        MAX_FREQUENCY := MAX(NATURAL_ARRAY(FREQUENCIES));
        if MAX_FREQUENCY <= MAX_BAR_LENGTH then
          COEFFICIENT := 1.0;
        else
          COEFFICIENT := FLOAT(MAX_BAR_LENGTH)/FLOAT(MAX_FREQUENCY);
        end if;
        for I in FREQUENCIES'RANGE loop
          PUT(I, WIDTH => 5);
          PUT(FREQUENCIES(I), WIDTH => 8);
          PUT("   ");
          for J in 1..INTEGER(COEFFICIENT * FLOAT(FREQUENCIES(I))) loop
            PUT('*');
          end loop;
          NEW_LINE;
        end loop;
        NEW_LINE;
        if EXACT then
          PUT("HAMMING NUMBER : ");
          PUT(HAMMING_NUMBER, WIDTH => 0);
          NEW_LINE;
          PUT("Errors corrected : ");
          PUT(ERRORS_CORRECTED, WIDTH => 0);
          NEW_LINE;
          PUT("Errors detected : ");
          PUT(ERRORS_DETECTED, WIDTH => 0);
          NEW_LINE;
        end if;
      end;
    end loop;
  end;
end TEST_BCH_CODE;
