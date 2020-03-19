with RANDOM_NUMERIC_TYPES,
     RANDOM,
     NUMBER_IMAGES,
     TEXT_IO,
     INTEGER_TEXT_IO,
     QUICK_SORT,
     CPU;

use RANDOM_NUMERIC_TYPES,
    RANDOM,
    TEXT_IO,
    INTEGER_TEXT_IO,
    CPU;

procedure TEST_QUICK_SORT is
-------------------------

  function GET_N return NATURAL;

  function GET_N return NATURAL is
    RESULT : NATURAL;
  begin
    PUT("Enter the array size : ");
    GET(RESULT);
    return RESULT;
  end GET_N;


begin
  RANDOMIZE;
  declare

    N : constant NATURAL := GET_N;

    type INT_ARRAY is array (POSITIVE range <>) of INTEGER;

    A : INT_ARRAY (1..N);

    B : INT_ARRAY (1..11) := (9, 4, 2, 5, 8, 7, 5, 1, 9, 4, 3);

    C : INT_ARRAY (1..11) := (others => 56);

    D : INT_ARRAY (1..11) := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

    OK : BOOLEAN := TRUE;

    procedure SORT is new QUICK_SORT(INDEX  => POSITIVE,
                                     ITEM   => INTEGER,
                                     VECTOR => INT_ARRAY);

    function IMAGE is new NUMBER_IMAGES.FIXED_IMAGE(DURATION);

  begin
    for I in A'RANGE loop
      A(I) := INTEGER(UNIFORM(-1012, 1012));
    end loop;
    SORT(A(1..0));  -- test if a null array is accepted
    SORT(B);
    SORT(D);
    SORT(C);
    START_COUNTER;
    SORT(A);
    PUT_LINE("CPU time used : " & IMAGE(CPU_TIME));
    for I in A'FIRST..A'LAST-1 loop
      if A(I) > A(I+1) then
        OK := FALSE;
      end if;
    end loop;
    if OK then
      PUT_LINE("The array is sorted");
    else
      PUT(ASCII.BEL);
      PUT_LINE("The array is not sorted");
    end if;
  end;
end;
