with USER_INTERFACE,
     TEXT_IO;

use USER_INTERFACE,
    TEXT_IO;

procedure TEST_USER_INTERFACE is

   type REAL is digits 20 range -1.0..44444444444444444444444444444444444.0;

   type FIX is delta 0.0001 range -7.0..17.77777777777777777777777;

   function REAL_ANSWER is new FLOAT_ANSWER(REAL);
   function FIX_ANSWER  is new FIXED_ANSWER(FIX);

   package REAL_TEXT_IO is new TEXT_IO.FLOAT_IO(REAL);
   package FIX_TEXT_IO  is new TEXT_IO.FIXED_IO(FIX);

   use REAL_TEXT_IO,
       FIX_TEXT_IO;

   X : REAL;
   F : FIX;

begin
   loop
      X := REAL_ANSWER("Enter a number for the type REAL : ");
      PUT(X);
      NEW_LINE;
      exit when X = 0.0;
   end loop;
   NEW_LINE;
   loop
      F := FIX_ANSWER("Enter a number for the type FIX : ");
      PUT(F);
      NEW_LINE;
      exit when F = 0.0;
   end loop;
end TEST_USER_INTERFACE;
