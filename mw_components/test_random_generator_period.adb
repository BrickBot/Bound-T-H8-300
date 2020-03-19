with RANDOM_GENERATOR_PERIOD,
     RANDOM_GENERATOR,
     NUMBER_IMAGES,
     TEXT_IO;

procedure TEST_RANDOM_GENERATOR_PERIOD is
--------------------------------------

   function IMAGE is new NUMBER_IMAGES.INTEGER_IMAGE(NATURAL);

begin
   declare

      generic
      package GENERATOR_10 is

         type VALUE is range 0..12;

         function RANDOM return VALUE;

      end GENERATOR_10;


      package body GENERATOR_10 is

         X : VALUE := VALUE'FIRST;

         function RANDOM return VALUE is
         begin
            if X < VALUE'LAST then
               X := X + 1;
            else
               X := 3;
            end if;
            return X;
         end;

      end GENERATOR_10;


      package GENERATOR_10_1 is new GENERATOR_10;
      package GENERATOR_10_2 is new GENERATOR_10;

      function EQUAL (X : GENERATOR_10_1.VALUE; Y : GENERATOR_10_2.VALUE) return BOOLEAN is
      begin
         return GENERATOR_10_1."="(X, GENERATOR_10_1.VALUE(Y));
      end EQUAL;

      function GENERATOR_10_PERIOD is new RANDOM_GENERATOR_PERIOD(RANDOM_NUMBER_1 => GENERATOR_10_1.VALUE,
                                                                  RANDOM_NUMBER_2 => GENERATOR_10_2.VALUE,
                                                                  EQUAL           => EQUAL,
                                                                  RANDOM_1        => GENERATOR_10_1.RANDOM,
                                                                  RANDOM_2        => GENERATOR_10_2.RANDOM,
                                                                  COUNT           => NATURAL,
                                                                  ZERO            => 0,
                                                                  SUCC            => NATURAL'SUCC);

   begin
      TEXT_IO.PUT_LINE("The period of GENERATOR_10 is " & IMAGE(GENERATOR_10_PERIOD));
   end;
   ---
   declare

      generic
      package GENERATOR_3 is

         type VALUE is range 0..12;

         function RANDOM return VALUE;

      end GENERATOR_3;


      package body GENERATOR_3 is

         X : VALUE := VALUE'FIRST;

         function RANDOM return VALUE is
         begin
            if X < VALUE'LAST then
               X := X + 1;
            else
               X := 10;
            end if;
            return X;
         end;

      end GENERATOR_3;


      package GENERATOR_3_1 is new GENERATOR_3;
      package GENERATOR_3_2 is new GENERATOR_3;

      function EQUAL (X : GENERATOR_3_1.VALUE; Y : GENERATOR_3_2.VALUE) return BOOLEAN is
      begin
         return GENERATOR_3_1."="(X, GENERATOR_3_1.VALUE(Y));
      end EQUAL;

      function GENERATOR_3_PERIOD is new RANDOM_GENERATOR_PERIOD(RANDOM_NUMBER_1 => GENERATOR_3_1.VALUE,
                                                                 RANDOM_NUMBER_2 => GENERATOR_3_2.VALUE,
                                                                 EQUAL           => EQUAL,
                                                                 RANDOM_1        => GENERATOR_3_1.RANDOM,
                                                                 RANDOM_2        => GENERATOR_3_2.RANDOM,
                                                                 COUNT           => NATURAL,
                                                                 ZERO            => 0,
                                                                 SUCC            => NATURAL'SUCC);

   begin
      TEXT_IO.PUT_LINE("The period of GENERATOR_3 is " & IMAGE(GENERATOR_3_PERIOD));
   end;
   ---
   declare

      package RANDOM_GENERATOR_1 is new RANDOM_GENERATOR;
      package RANDOM_GENERATOR_2 is new RANDOM_GENERATOR;

      function EQUAL (X : RANDOM_GENERATOR_1.RANDOM_INTEGER;
                      Y : RANDOM_GENERATOR_2.RANDOM_INTEGER) return BOOLEAN is
      begin
         return RANDOM_GENERATOR_1."="(X, RANDOM_GENERATOR_1.RANDOM_INTEGER(Y));
      end EQUAL;

      function RANDOM_PERIOD is
         new RANDOM_GENERATOR_PERIOD(RANDOM_NUMBER_1 => RANDOM_GENERATOR_1.RANDOM_INTEGER,
                                     RANDOM_NUMBER_2 => RANDOM_GENERATOR_2.RANDOM_INTEGER,
                                     EQUAL           => EQUAL,
                                     RANDOM_1        => RANDOM_GENERATOR_1.UNIFORM,
                                     RANDOM_2        => RANDOM_GENERATOR_2.UNIFORM,
                                     COUNT           => NATURAL,
                                     ZERO            => 0,
                                     SUCC            => NATURAL'SUCC);

   begin
      TEXT_IO.PUT_LINE("The period of RANDOM_GENERATOR is " & IMAGE(RANDOM_PERIOD));
   end;
end TEST_RANDOM_GENERATOR_PERIOD;
