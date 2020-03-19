with CALENDAR,
     EXTENDED_CALENDAR,
     NUMBER_IMAGES,
     TEXT_IO,
     INTEGER_TEXT_IO,
     USER_INTERFACE;

use CALENDAR,
    EXTENDED_CALENDAR,
    TEXT_IO,
    INTEGER_TEXT_IO,
    USER_INTERFACE;

procedure TEST_EXTENDED_CALENDAR is
--------------------------------

   function IMAGE is new NUMBER_IMAGES.FIXED_IMAGE(DURATION);
   function IMAGE is new NUMBER_IMAGES.INTEGER_IMAGE(DAY_INTERVAL);
   function IMAGE is new NUMBER_IMAGES.INTEGER_IMAGE(INTEGER);

   function IMAGE (VALUE : LONG_DURATION) return STRING is
   begin
      return "(" & DURATION_SIGN'IMAGE(SIGN(VALUE)) & ", " &
             IMAGE(DAYS(VALUE)) & ", " &
             IMAGE(SECONDS(VALUE)) & ")";
   end IMAGE;


   function INT_ANSWER is new INTEGER_ANSWER(INTEGER);


   function LONG_DURATION_ANSWER (PROMPT : STRING) return LONG_DURATION is

      function DAY_INTERVAL_ANSWER is new INTEGER_ANSWER(DAY_INTERVAL);
      function DURATION_ANSWER     is new FIXED_ANSWER(DURATION);

      function SIGN_ANSWER (PROMPT : STRING) return DURATION_SIGN is
      begin
         loop
            declare

               ANSWER : constant STRING := STRING_ANSWER(PROMPT, NULL_STRING_ALLOWED => TRUE);

            begin
               if ANSWER = "+" or ANSWER = "" then
                  return '+';
               elsif ANSWER = "-" then
                  return '-';
               end if;
            end;
         end loop;
      end SIGN_ANSWER;

   begin
      PUT_LINE(PROMPT);
      declare

         DAYS    : constant DAY_INTERVAL  := DAY_INTERVAL_ANSWER("   Days    => ");
         SECONDS : constant DURATION      := DURATION_ANSWER    ("   Seconds => ");
         SIGN    : constant DURATION_SIGN := SIGN_ANSWER        ("   Sign    => ");

      begin
         return TO_LONG_DURATION(DAYS, SECONDS, SIGN);
      end;
   end LONG_DURATION_ANSWER;

begin
   PUT_LINE("Test of function TO_DURATION");
   while YES_NO_ANSWER("More ? ") loop
      declare

         L : constant LONG_DURATION := LONG_DURATION_ANSWER("Enter a long duration :");

      begin
         PUT_LINE("Duration value of " & IMAGE(L) & " is " & IMAGE(TO_DURATION(L)));
      exception
         when NUMERIC_ERROR =>
            PUT_LINE("   -- NUMERIC_ERROR was raised");
         when CONSTRAINT_ERROR =>
            PUT_LINE("   -- CONSTRAINT_ERROR was raised");
      end;
   end loop;
   ---
   NEW_LINE;
   PUT_LINE("Test of function ""+""(LONG_DURATION, LONG_DURATION)");
   while YES_NO_ANSWER("More ? ") loop
      declare

         L1 : constant LONG_DURATION := LONG_DURATION_ANSWER("Enter a long duration :");
         L2 : constant LONG_DURATION := LONG_DURATION_ANSWER("Enter another long duration :");

      begin
         PUT_LINE(IMAGE(L1) & " + " & IMAGE(L2) & " = " & IMAGE(L1 + L2));
      exception
         when NUMERIC_ERROR =>
            PUT_LINE("   -- NUMERIC_ERROR was raised");
         when CONSTRAINT_ERROR =>
            PUT_LINE("   -- CONSTRAINT_ERROR was raised");
      end;
   end loop;
   ---
   NEW_LINE;
   PUT_LINE("Test of function ""-""(LONG_DURATION, LONG_DURATION)");
   while YES_NO_ANSWER("More ? ") loop
      declare

         L1 : constant LONG_DURATION := LONG_DURATION_ANSWER("Enter a long duration :");
         L2 : constant LONG_DURATION := LONG_DURATION_ANSWER("Enter another long duration :");

      begin
         PUT_LINE(IMAGE(L1) & " - " & IMAGE(L2) & " = " & IMAGE(L1 - L2));
      exception
         when NUMERIC_ERROR =>
            PUT_LINE("   -- NUMERIC_ERROR was raised");
         when CONSTRAINT_ERROR =>
            PUT_LINE("   -- CONSTRAINT_ERROR was raised");
      end;
   end loop;
   ---
   NEW_LINE;
   PUT_LINE("Test of function ""*""(INTEGER, LONG_DURATION)");
   while YES_NO_ANSWER("More ? ") loop
      declare

         N : constant INTEGER       := INT_ANSWER("Enter an integer : ");
         L : constant LONG_DURATION := LONG_DURATION_ANSWER("Enter a long duration :");

      begin
         PUT_LINE(IMAGE(N) & " * " & IMAGE(L) & " = " & IMAGE(N * L));
      exception
         when NUMERIC_ERROR =>
            PUT_LINE("   -- NUMERIC_ERROR was raised");
         when CONSTRAINT_ERROR =>
            PUT_LINE("   -- CONSTRAINT_ERROR was raised");
      end;
   end loop;
   ---
   NEW_LINE;
   PUT_LINE("Test of function ""/""(LONG_DURATION, INTEGER)");
   while YES_NO_ANSWER("More ? ") loop
      declare

         L : constant LONG_DURATION := LONG_DURATION_ANSWER("Enter a long duration :");
         N : constant INTEGER       := INT_ANSWER("Enter an integer : ");

      begin
         PUT_LINE(IMAGE(L) & " / " & IMAGE(N) & " = " & IMAGE(L / N));
      exception
         when NUMERIC_ERROR =>
            PUT_LINE("   -- NUMERIC_ERROR was raised");
         when CONSTRAINT_ERROR =>
            PUT_LINE("   -- CONSTRAINT_ERROR was raised");
      end;
   end loop;
   ---
   NEW_LINE;
   PUT_LINE("Test of functions ""<""(LONG_DURATION, LONG_DURATION) and ""<=""(LONG_DURATION, LONG_DURATION)");
   while YES_NO_ANSWER("More ? ") loop
      declare

         L1 : constant LONG_DURATION := LONG_DURATION_ANSWER("Enter a long duration :");
         L2 : constant LONG_DURATION := LONG_DURATION_ANSWER("Enter another long duration :");

      begin
         PUT_LINE(IMAGE(L1) & " <  " & IMAGE(L2) & " = " & BOOLEAN'IMAGE(L1 < L2));
         PUT_LINE(IMAGE(L1) & " <= " & IMAGE(L2) & " = " & BOOLEAN'IMAGE(L1 <= L2));
         PUT_LINE(IMAGE(L1) & " >  " & IMAGE(L2) & " = " & BOOLEAN'IMAGE(L1 > L2));
         PUT_LINE(IMAGE(L1) & " >= " & IMAGE(L2) & " = " & BOOLEAN'IMAGE(L1 >= L2));
      end;
   end loop;
   ---
   NEW_LINE;
   PUT_LINE("Test of function ""+""(TIME, LONG_DURATION)");
   while YES_NO_ANSWER("More ? ") loop
      declare

         L : constant LONG_DURATION := LONG_DURATION_ANSWER("Enter a long duration :");
         D : constant TIME := TIME_OF(YEAR => 1901, MONTH => 1, DAY => 1) + L;

      begin
         PUT_LINE("1-1-1901 + " & IMAGE(L) & " = " &
                  IMAGE(DAY(D)) & "-" & IMAGE(MONTH(D)) & "-" & IMAGE(YEAR(D)) & ":" & IMAGE(SECONDS(D)));
      end;
   end loop;
   ---
   NEW_LINE;
   PUT_LINE("Test of function DAY_OF_WEEK");
   declare

      YEAR  : YEAR_NUMBER;
      MONTH : MONTH_NUMBER;
      DAY   : DAY_NUMBER;

   begin
      PUT("Today is ");
      PUT(WEEK_DAY'IMAGE(DAY_OF_WEEK));
      NEW_LINE;
      loop
         PUT("Enter a date (d m y) : ");
         GET(DAY); GET(MONTH); GET(YEAR);
         SKIP_LINE;
         PUT(WEEK_DAY'IMAGE(DAY_OF_WEEK(YEAR, MONTH, DAY)));
         NEW_LINE;
      end loop;
   end;
end TEST_EXTENDED_CALENDAR;
