with TEXT_IO,
     INTEGER_TEXT_IO,
     QUICK_SORT,
     BAGS,
     THE_RANDOM_GENERATOR,
     RANDOM_NUMBERS,
     CPU;

use TEXT_IO,
    INTEGER_TEXT_IO,
    CPU;

procedure TEST_CPU is

  package DURATION_TEXT_IO is new FIXED_IO(DURATION);
  use DURATION_TEXT_IO;

  type I_1012 is range -1012..1013;

  package RANDOM_1012 is new RANDOM_NUMBERS(I_1012,
                                            LONG_FLOAT,
                                            THE_RANDOM_GENERATOR.RANDOM_REAL,
                                            THE_RANDOM_GENERATOR.UNIFORM,
                                            THE_RANDOM_GENERATOR.RESET,
                                            THE_RANDOM_GENERATOR.RANDOMIZE);

  function GET_N (PROMPT : in STRING) return INTEGER is

    N : INTEGER;

  begin
    PUT(PROMPT);
    GET(N);
    return N;
  end GET_N;

begin
  RANDOM_1012.RANDOMIZE;
  declare

    type I_ARRAY is array (POSITIVE range <>) of I_1012;

    N : constant NATURAL := GET_N("Enter array size : ");
    A : I_ARRAY(1..N) := (others => RANDOM_1012.UNIFORM(I_1012'FIRST,
                                                        I_1012'LAST-1));

    procedure SORT is new QUICK_SORT(INDEX  => POSITIVE,
                                     ITEM   => I_1012,
                                     VECTOR => I_ARRAY);

    function IDENTITY (X : I_1012) return I_1012;

    package BAG_1012 is new BAGS(I_1012, I_1012, IDENTITY, COUNT => INTEGER);
    use BAG_1012;

    T : BAG(DUPLICATE_KEYS_ALLOWED => TRUE);

    QUICK_SORT_CPU,
    TREE_SORT_CPU   : CPU_COUNTER;


    function IDENTITY (X : I_1012) return I_1012 is
    begin
      return X;
    end;

  begin
    START_COUNTER(QUICK_SORT_CPU);
    SORT(A);
    PUT("Quick sort CPU time : ");
    PUT(CPU_TIME(QUICK_SORT_CPU));
    NEW_LINE;
    --
    A := (others => RANDOM_1012.UNIFORM(I_1012'FIRST, I_1012'LAST-1));
    START_COUNTER(TREE_SORT_CPU);
    INSERT(LIST(A), T);
    A := I_ARRAY(TO_LIST(T));
    DESTROY(T);
    PUT("Tree sort CPU time : ");
    PUT(CPU_TIME(TREE_SORT_CPU));
    NEW_LINE;
    --
    PUT("Total CPU time : ");
    PUT(CPU_TIME);
    NEW_LINE;
    PUT("Process CPU time : ");
    PUT(PROCESS_CPU_TIME);
    NEW_LINE;
  end;
end TEST_CPU;
