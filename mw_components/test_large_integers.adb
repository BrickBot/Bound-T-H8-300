with LARGE_INTEGERS,
     NUMBER_IMAGES,
     TEXT_IO,
     USER_INTERFACE;

use TEXT_IO;

procedure TEST_LARGE_INTEGERS is
-----------------------------

   function POSITIVE_ANSWER is new USER_INTERFACE.INTEGER_ANSWER(POSITIVE);


   type SLICE_VALUE is new INTEGER range 0..POSITIVE_ANSWER("Base : ") - 1;

   type SLICE_ARITHMETIC is new INTEGER range 0..INTEGER(SLICE_VALUE'LAST) ** 2 + 2 * INTEGER(SLICE_VALUE'LAST);

   type SLICE_INDEX is new INTEGER range 0..POSITIVE_ANSWER("Slice_Index'Last : ");


   package LARGE_INTEGER_HANDLER is new LARGE_INTEGERS (SLICE_NUMBER      => SLICE_VALUE,
                                                        ARITHMETIC_NUMBER => SLICE_ARITHMETIC,
                                                        SLICE_INDEX       => SLICE_INDEX);

   use LARGE_INTEGER_HANDLER,
       LARGE_INTEGER_HANDLER.VARIABLE_LENGTH_OPERATIONS;


   function TO_LARGE_INTEGER is new VARIABLE_LENGTH_OPERATIONS.INTEGER_TO_LARGE_INTEGER(INTEGER);
   function TO_INTEGER       is new LARGE_INTEGER_TO_INTEGER(INTEGER);

   X, Y : LARGE_INTEGER;
   EXP  : NATURAL;


   function IMAGE is new NUMBER_IMAGES.INTEGER_IMAGE(NATURAL);
   function NATURAL_ANSWER is new USER_INTERFACE.INTEGER_ANSWER(NATURAL);


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


begin
   if USER_INTERFACE.YES_NO_ANSWER("Perform exhaustive test of division ? ") then
      declare

         X_LOWER_BOUND : constant LARGE_INTEGER := LARGE_INTEGER_ANSWER("Lower Bound on Left Operand  : ");
         X_UPPER_BOUND : constant LARGE_INTEGER := LARGE_INTEGER_ANSWER("Upper Bound on Left Operand  : ");
         Y_LOWER_BOUND : constant LARGE_INTEGER := LARGE_INTEGER_ANSWER("Lower Bound on Right Operand : ");
         Y_UPPER_BOUND : constant LARGE_INTEGER := LARGE_INTEGER_ANSWER("Upper Bound on Right Operand : ");

         FAILED        : BOOLEAN := FALSE;

      begin
         X := X_LOWER_BOUND;
         while X <= X_UPPER_BOUND loop
            Y := Y_LOWER_BOUND;
            while Y <= Y_UPPER_BOUND loop
               if Y /= ZERO and then (X / Y) * Y + X rem Y /= X then
                  FAILED := TRUE;
                  PUT_LINE("Test failed with X = " & IMAGE(X) & ", Y = " & IMAGE(Y));
                  PUT_LINE(IMAGE(X) & " / " & IMAGE(Y) & " = " & IMAGE(X / Y));
                  PUT_LINE(IMAGE(X) & " rem " & IMAGE(Y) & " = " & IMAGE(X rem Y));
                  NEW_LINE;
               end if;
               Y := Y + ONE;
            end loop;
            X := X + ONE;
         end loop;
         if not FAILED then
            PUT_LINE("All tests succeeded");
         end if;
      end;
   end if;
   --
   loop
      X := LARGE_INTEGER_ANSWER("Enter X : ");
      Y := LARGE_INTEGER_ANSWER("Enter Y : ");
      PUT_LINE(IMAGE(X) & " + " & IMAGE(Y) & " = " & IMAGE(X + Y));
      PUT_LINE(IMAGE(X) & " - " & IMAGE(Y) & " = " & IMAGE(X - Y));
      PUT_LINE(IMAGE(X) & " * " & IMAGE(Y) & " = " & IMAGE(X * Y));
      begin
         PUT_LINE(IMAGE(X) & " / " & IMAGE(Y) & " = " & IMAGE(X / Y));
      exception
         when DIVISION_BY_ZERO =>
            PUT_LINE("DIVISION_BY_ZERO raised");
      end;
      begin
         PUT_LINE(IMAGE(X) & " mod " & IMAGE(Y) & " = " & IMAGE(X mod Y));
      exception
         when DIVISION_BY_ZERO =>
            PUT_LINE("DIVISION_BY_ZERO raised");
      end;
      begin
         PUT_LINE(IMAGE(X) & " rem " & IMAGE(Y) & " = " & IMAGE(X rem Y));
      exception
         when DIVISION_BY_ZERO =>
            PUT_LINE("DIVISION_BY_ZERO raised");
      end;
      exit when X = ZERO and Y = ZERO;
   end loop;
   --
   NEW_LINE;
   loop
      X := LARGE_INTEGER_ANSWER("Enter X : ");
      EXP := NATURAL_ANSWER("Enter exponent : ");
      PUT_LINE(IMAGE(X) & " ** " & IMAGE(EXP) & " = " & IMAGE(X ** EXP));
      exit when X = ZERO and EXP = 0;
   end loop;
   PUT_LINE(IMAGE(TO_LARGE_INTEGER(TO_INTEGER(TO_LARGE_INTEGER(7682734)))));
end TEST_LARGE_INTEGERS;
