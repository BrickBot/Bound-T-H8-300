with TEXT_IO,
     INTEGER_TEXT_IO,
     POLYNOMIALS;

use TEXT_IO,
    INTEGER_TEXT_IO;

procedure TEST_POLYNOMIALS is

  package INTEGER_POLYNOMIALS is new POLYNOMIALS(INTEGER,
                                                 INTEGER,
                                                 ZERO => 0,
                                                 ONE  => 1 );

  use INTEGER_POLYNOMIALS;


  function GET_POLYNOMIAL (PROMPT : STRING) return POLYNOMIAL is

    DEG : DEGREE;

  begin
    PUT_LINE(PROMPT);
    PUT("Degree : "); GET(DEG); SKIP_LINE;
    declare
      P : POLYNOMIAL(0..DEG);
    begin
      for I in P'RANGE loop
        PUT("Coefficient of x^"); PUT(I, WIDTH => 0); PUT(" : ");
        GET(P(I)); SKIP_LINE;
      end loop;
      return P;
    end;
  end GET_POLYNOMIAL;


  procedure PUT (P : in POLYNOMIAL) is
  begin
    if DEG(P) = -1 then
      PUT(" 0");
    else
      for I in P'RANGE loop
        if P(I) /= 0 then
          if P(I) >= 0 then
            PUT(" + ");
          else
            PUT(" - ");
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

begin
  loop
    declare
      P : constant POLYNOMIAL := GET_POLYNOMIAL("ENTER P");
      Q : constant POLYNOMIAL := GET_POLYNOMIAL("ENTER Q");
    begin
      PUT("P(x) = "); PUT(P); NEW_LINE;
      PUT("Q(x) = "); PUT(Q); NEW_LINE;
      PUT("P(x) + Q(x) = "); PUT(P+Q); NEW_LINE;
      PUT("P(x) - Q(x) = "); PUT(P-Q); NEW_LINE;
      PUT("P(x) * Q(x) = "); PUT(P*Q); NEW_LINE;
      PUT("P(x) / Q(x) = "); PUT(P/Q); NEW_LINE;
      PUT("P(x) mod Q(x) = "); PUT(P mod Q); NEW_LINE;
    end;
  end loop;
end TEST_POLYNOMIALS;
