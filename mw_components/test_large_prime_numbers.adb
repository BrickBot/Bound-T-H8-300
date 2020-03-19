with LARGE_INTEGER_HANDLER,
     PRIME_NUMBERS,
     NUMBER_IMAGES,
     TEXT_IO,
     USER_INTERFACE;

use TEXT_IO;

procedure TEST_LARGE_PRIME_NUMBERS is
----------------------------------

   package FIXED_LENGTH_NUMBERS is new LARGE_INTEGER_HANDLER.FIXED_LENGTH_OPERATIONS(LENGTH => 15);

   use LARGE_INTEGER_HANDLER,
       FIXED_LENGTH_NUMBERS;

   function TO_INTEGER       is new LARGE_INTEGER_TO_INTEGER(INTEGER);
   function TO_LARGE_INTEGER is new INTEGER_TO_LARGE_INTEGER(INTEGER);


   SQRT_OF_NEGATIVE_VALUE : exception;

   function SQRT (X : FIXED_LENGTH_LARGE_INTEGER) return FIXED_LENGTH_LARGE_INTEGER is

      U     : FIXED_LENGTH_LARGE_INTEGER := X;
      NEW_U : FIXED_LENGTH_LARGE_INTEGER;

      TWO   : constant FIXED_LENGTH_LARGE_INTEGER := ONE + ONE;

   begin
      if X < ZERO then
         raise SQRT_OF_NEGATIVE_VALUE;
      end if;
      if X = ZERO then
         return ZERO;
      end if;
      loop
         NEW_U := (U + X/U)/TWO;
         exit when NEW_U >= U;
         U := NEW_U;
      end loop;
      return U;
   end SQRT;


   package LARGE_INTEGER_PRIMES is new PRIME_NUMBERS(INT        => FIXED_LENGTH_LARGE_INTEGER,
                                                     ZERO       => ZERO,
                                                     ONE        => ONE,
                                                     SQRT       => SQRT,
                                                     TO_INTEGER => TO_INTEGER);

   use LARGE_INTEGER_PRIMES;


   function LARGE_INTEGER_ANSWER (PROMPT : STRING) return LARGE_INTEGER is
   begin
      loop
         begin
            return VALUE(USER_INTERFACE.STRING_ANSWER(PROMPT,
                                                      STRIP_LEADING_BLANKS  => FALSE,
                                                      STRIP_TRAILING_BLANKS => FALSE));
         exception
            when CONSTRAINT_ERROR =>
               null;
         end;
      end loop;
   end LARGE_INTEGER_ANSWER;


   function IMAGE is new NUMBER_IMAGES.INTEGER_IMAGE(NATURAL);

begin
   loop
      declare

         N : constant FIXED_LENGTH_LARGE_INTEGER := +LARGE_INTEGER_ANSWER("Enter a number : ");

      begin
         PUT(IMAGE(N));
         if PRIME(N) then
            PUT(" is prime");
         else
            PUT(" is not prime");
         end if;
         NEW_LINE;
         declare

            N_DECOMPOSED : constant ARRAY_OF_POWER := FACTORIZATION(N);

         begin
            PUT(IMAGE(N) & " = ");
            for I in N_DECOMPOSED'RANGE loop
               PUT(IMAGE(N_DECOMPOSED(I).BASE));
               if N_DECOMPOSED(I).EXPONENT /= 1 then
                  PUT('^' & IMAGE(N_DECOMPOSED(I).EXPONENT));
               end if;
               if I < N_DECOMPOSED'LAST then
                  PUT(" * ");
               end if;
            end loop;
            NEW_LINE;
         end;
         exit when N = ZERO;
      end;
   end loop;
   loop
      NEW_LINE;
      declare

         FIRST : constant FIXED_LENGTH_LARGE_INTEGER := +LARGE_INTEGER_ANSWER("First : ");
         LAST  : constant FIXED_LENGTH_LARGE_INTEGER := +LARGE_INTEGER_ANSWER("Last  : ");

         THE_PRIMES : constant INT_ARRAY := PRIMES(FROM => FIRST, TO => LAST);

      begin
         for I in THE_PRIMES'RANGE loop
            PUT_LINE(IMAGE(THE_PRIMES(I)));
         end loop;
         exit when FIRST = ZERO and LAST = ZERO;
      end;
   end loop;
   NEW_LINE;
   declare

      N : FIXED_LENGTH_LARGE_INTEGER := TO_LARGE_INTEGER(10);

   begin
      loop
         PUT("First prime following " & IMAGE(N) & " : ");
         while not PRIME(N) loop
            N := N + ONE;
         end loop;
         PUT(IMAGE(N));
         NEW_LINE;
         N := N * TO_LARGE_INTEGER(10);
      end loop;
   end;
end TEST_LARGE_PRIME_NUMBERS;
