with FAST_GALOIS_FIELD,
     TEXT_IO,
     INTEGER_TEXT_IO;

use TEXT_IO,
    INTEGER_TEXT_IO;

procedure TEST_FAST_GALOIS_FIELD is

  function GET_INTEGER (PROMPT : STRING) return INTEGER is

    N : INTEGER;

  begin
    PUT(PROMPT);
    GET(N);
    SKIP_LINE;
    return N;
  end;

begin
  declare

    P : constant POSITIVE := GET_INTEGER("Enter p : ");
    N : constant POSITIVE := GET_INTEGER("Enter n : ");

    package GALOIS_FIELD_P_N is new FAST_GALOIS_FIELD(P,N);

    use GALOIS_FIELD_P_N,
        GALOIS_FIELD_P_N.BASE_ZPZ_FIELD,
        GALOIS_FIELD_P_N.BASE_ZPZ_FIELD.ZPZ_POLYNOMIALS;

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


    function GET_ELEMENT (PROMPT : STRING) return ELEMENT is

      N : INTEGER;

    begin
      PUT(PROMPT & "(<ret> for ZERO) ");
      if END_OF_LINE then
        SKIP_LINE;
        return ZERO;
      else
        GET(N);
        return GENERATOR**N;
      end if;
    end;


  begin
    NEW_LINE;
    PUT_LINE("Test function IRREDUCIBLE");
    PUT     ("-------------------------");
    loop
      NEW_LINE;
      declare
        P : constant POLYNOMIAL := GET_POLYNOMIAL(PROMPT => "ENTER P");
      begin
        PUT(P);
        if IRREDUCIBLE(P) then
          PUT(" IS IRREDUCIBLE");
        else
          PUT(" IS REDUCIBLE");
        end if;
        NEW_LINE;
        exit when DEG(P) = -1;
      end;
    end loop;
    --
    NEW_LINE;
    PUT_LINE("Test procedure ENUMERATE_IRREDUCIBLES");
    PUT     ("-------------------------------------");
    declare

      M : DEGREE;

      RANK : NATURAL;

      procedure PUT_POLYNOMIAL_AND_NUMBER (P : in POLYNOMIAL) is
      begin
        RANK := RANK + 1;
        PUT(RANK, WIDTH => 4);
        PUT(")  ");
        PUT(P);
        NEW_LINE;
      end;

      procedure PUT_ALL_IRREDUCIBLES is
        new ENUMERATE_IRREDUCIBLES(PUT_POLYNOMIAL_AND_NUMBER);

    begin
      loop
        NEW_LINE;
        PUT("Enter degree : ");
        GET(M); SKIP_LINE;
        PUT("Irreducible polynomials of degree ");
        PUT(M, WIDTH => 0); PUT(" : "); NEW_LINE;
        RANK := 0;
        PUT_ALL_IRREDUCIBLES(M);
        exit when M = -1;
      end loop;
    end;
    --
    NEW_LINE;
    PUT_LINE("Test function INVERSE(ELEMENT)");
    PUT     ("------------------------------");
    declare
      A : ELEMENT;
    begin
      loop
        NEW_LINE;
        A := GET_ELEMENT("Enter element to inverse : ");
        exit when A = ZERO;
        PUT("INVERSE OF ");
        PUT(TO_POLYNOMIAL(A));
        PUT(" : ");
        PUT(TO_POLYNOMIAL(INVERSE(A)));
        if A * INVERSE(A) /= ONE then
          PUT_LINE("*************** INVERSION FAILED *************");
        end if;
        NEW_LINE;
      end loop;
    end;
    --
    NEW_LINE;
    PUT_LINE("Test function MINIMAL_POLYNOMIAL(ELEMENT)");
    PUT     ("-----------------------------------------");
    declare
      A : ELEMENT;
    begin
      loop
        NEW_LINE;
        A := GET_ELEMENT("Enter Element");
        PUT("MINIMAL POLYNOMIAL OF ");
        PUT(TO_POLYNOMIAL(A));
        PUT(" : ");
        PUT(MINIMAL_POLYNOMIAL(A));
        NEW_LINE;
        exit when A = ZERO;
      end loop;
    end;
    --
    NEW_LINE;
    PUT_LINE("Test function GENERATORS");
    PUT     ("------------------------");
    declare

      GEN : constant ELEMENT_ARRAY := GENERATORS;

    begin
      NEW_LINE;
      for I in GEN'RANGE loop
        PUT(I, WIDTH => 4); PUT(") ");
        PUT(TO_POLYNOMIAL(GEN(I)));
        NEW_LINE;
      end loop;
    end;
  end;
end TEST_FAST_GALOIS_FIELD;
