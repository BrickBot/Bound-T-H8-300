with LARGE_INTEGER_HANDLER,
     NUMBER_IMAGES,
     TEXT_IO,
     USER_INTERFACE;

use TEXT_IO;

procedure TEST_LARGE_INTEGER_HANDLER is
------------------------------------

   use LARGE_INTEGER_HANDLER,
       LARGE_INTEGER_HANDLER.VARIABLE_LENGTH_OPERATIONS;


   function TO_LARGE_INTEGER is new VARIABLE_LENGTH_OPERATIONS.INTEGER_TO_LARGE_INTEGER(INTEGER);
   function TO_INTEGER       is new LARGE_INTEGER_TO_INTEGER(INTEGER);

   X, Y : LARGE_INTEGER(30);


   function IMAGE is new NUMBER_IMAGES.INTEGER_IMAGE(INTEGER);
   function INT_ANSWER is new USER_INTERFACE.INTEGER_ANSWER(INTEGER);


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
         ASSIGN(X, VALUE => X_LOWER_BOUND);
         while X <= X_UPPER_BOUND loop
            ASSIGN(Y, VALUE => Y_LOWER_BOUND);
            while Y <= Y_UPPER_BOUND loop
               if not EQUAL(Y, ZERO) and then not EQUAL((X / Y) * Y + X rem Y, X) then
                  FAILED := TRUE;
                  PUT_LINE("Test failed with X = " & IMAGE(X) & ", Y = " & IMAGE(Y));
                  PUT_LINE(IMAGE(X) & " / " & IMAGE(Y) & " = " & IMAGE(X / Y));
                  PUT_LINE(IMAGE(X) & " rem " & IMAGE(Y) & " = " & IMAGE(X rem Y));
                  NEW_LINE;
               end if;
               ASSIGN(Y, VALUE => Y + ONE);
            end loop;
            ASSIGN(X, VALUE => X + ONE);
         end loop;
         if not FAILED then
            PUT_LINE("All tests succeeded");
         end if;
      end;
   end if;
   --
   loop
      ASSIGN(X, LARGE_INTEGER_ANSWER("Enter X : "));
      ASSIGN(Y, LARGE_INTEGER_ANSWER("Enter Y : "));
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
      exit when EQUAL(X, ZERO) and EQUAL(Y, ZERO);
   end loop;
   --
   PUT_LINE(IMAGE(TO_LARGE_INTEGER(TO_INTEGER(TO_LARGE_INTEGER(7682734)))));
end TEST_LARGE_INTEGER_HANDLER;
