with LONG_FLOAT_COMPLEX,
     TEXT_IO,
     LONG_FLOAT_TEXT_IO;

use LONG_FLOAT_COMPLEX,
    TEXT_IO,
    LONG_FLOAT_TEXT_IO;

procedure TEST_COMPLEX is

  Z : COMPLEX;

  procedure GET (X : out COMPLEX) is
  begin
    GET(X.RE); GET(X.IM);
  end GET;

  procedure PUT (X : in COMPLEX) is
  begin
    PUT(X.RE);
    PUT("+");
    PUT(X.IM);
    PUT("i");
  end PUT;

begin
  LONG_FLOAT_TEXT_IO.DEFAULT_FORE := 0;
  LONG_FLOAT_TEXT_IO.DEFAULT_AFT  := 2;
  LONG_FLOAT_TEXT_IO.DEFAULT_EXP  := 0;
  loop
    PUT("Enter Z : "); GET(Z);
    PUT("arg Z = "); PUT(ARG(Z)); NEW_LINE;
  end loop;
end TEST_COMPLEX;
