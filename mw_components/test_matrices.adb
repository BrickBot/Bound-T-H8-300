with MATHEMATICAL_CONSTANTS,
     MATRICES,
     LONG_FLOAT_COMPLEX;

use MATHEMATICAL_CONSTANTS,
    LONG_FLOAT_COMPLEX;

procedure TEST_MATRICES is

  E : constant := NATURAL_E;

begin
  declare

    package FLOAT_MATRICES is new MATRICES(INDEX  => POSITIVE,
                                           SCALAR => FLOAT,
                                           ZERO   => 0.0,
                                           ONE    => 1.0 );

    use FLOAT_MATRICES;

    -- package COMPLEX_MATRICES is new MATRICES(INDEX  => NATURAL,
    --                                          SCALAR => COMPLEX,
    --                                          ZERO   => (0.0,0.0),
    --                                          ONE    => (1.0,0.0) );

    A : constant MATRIX := (( 2.0, 5.0, 6.0),
                            (-1.0, 7.0, 4.0));

    B : constant MATRIX := ((-1.0, 7.0),
                            ( 0.0, 6.0),
                            (-2.0, 3.0),
                            (10.0,-2.0));

    I : constant MATRIX := IDENTITY(1,4);

    C : constant MATRIX := B*A;

    D : constant MATRIX := I*B;

    E : constant MATRIX := A+A;

    M : constant MATRIX := ((1.0,2.0,2.0),
                            (2.0,5.0,1.0),
                            (1.0,0.0,1.0));

    DET_M : constant FLOAT := DET(M);

    W : constant MATRIX := ((1.0,2.0,2.0,3.0,4.0),
                            (2.0,5.0,1.0,1.0,0.0),
                            (1.0,0.0,1.0,7.0,8.0),
                            (2.0,4.0,4.0,6.0,8.0),
                            (4.0,7.0,9.0,4.0,7.0));   -- singular

    DET_W : constant FLOAT := DET(W);

    H : constant MATRIX := ((1.0,2.0,2.0,3.0,4.0),
                            (2.0,4.0,4.0,6.0,8.0),
                            (2.0,4.0,4.0,6.0,8.0),
                            (2.0,4.0,4.0,6.0,8.0),
                            (2.0,4.0,4.0,6.0,8.0));   -- singular

    DET_H : constant FLOAT := DET(H);

  begin
    null;
  end;
  --
  declare

    type BOLZNOI is (LEMNO, ZNOL, KOLMOR, MOZ, GALNAZ);

    type ZINK is digits 20;

    package ZINKBOLMAT is new MATRICES(INDEX  => BOLZNOI,
                                       SCALAR => ZINK,
                                       ZERO   => 0.0,
                                       ONE    => 1.0 );

    use ZINKBOLMAT;

    NONO : constant MATRIX := (( 1.0, 2.0, 3.0,-1.0, 4.0),
                               ( 2.0, 4.0, 7.0, 2.0,16.0),
                               ( 4.0, 8.0,13.0, 0.0,23.0),
                               ( 3.0, 5.0, 9.0,-3.0,12.0),
                               ( 7.0,10.0,23.0,-2.0,38.0) );

    HAL : constant VECTOR := NONO * (2.0,-1.0,0.0,3.0,-4.0);

    MOQ : constant VECTOR := LINEAR_SYSTEM_SOLUTION(NONO,HAL);

    DET_NONO : constant ZINK := DET(NONO);

    INV_NONO : constant MATRIX := 1.0/NONO;

    NOLE : constant MATRIX := NONO * INV_NONO;

    LOMO : constant MATRIX := ((17.1,12.0,-3.0,-1.6,14.5),
                               ( 2.2,14.3,17.6,-2.0, 0.0),
                               (-4.6,-8.0,13.0, 0.7, 3.0),
                               (-3.0,21.0,-9.0,-3.3, 2.0),
                               ( 7.5,10.1,23.9,-2.0,38.1) );

    INV_LOMO : constant MATRIX := INVERSE(LOMO);

    NALGOZ : constant MATRIX (BOLZNOI, KOLMOR..GALNAZ) := ((  PI,   E, 0.0),
                                                           (-3.0,-2.0, 0.0),
                                                           (-2.0,-7.0, 0.0),
                                                           (  -E,14.0, 0.0),
                                                           ( 7.0, 7.0, 0.0) );

    NOLZERO : constant MATRIX := LOMO * LINEAR_SYSTEM_SOLUTION(LOMO,NALGOZ) - NALGOZ;

    NOLID : constant MATRIX := LOMO/LOMO;

  begin
    null;
  end;
  --
  declare

    type KOLZ is range 17..17;

    package BALZ is new MATRICES(INDEX  => KOLZ,
                                 SCALAR => FLOAT,
                                 ZERO   => 0.0,
                                 ONE    => 1.0 );

    use BALZ;

    A : constant MATRIX := INVERSE((17 => (17 => 0.6)));

    V : constant VECTOR := LINEAR_SYSTEM_SOLUTION((17 => (17 => (0.2))), (17 => 0.7));

    B : constant MATRIX := LINEAR_SYSTEM_SOLUTION((17 => (17 => (0.2))), (17 => (17 => 0.7)));

  begin
    null;
  end;
end TEST_MATRICES;
