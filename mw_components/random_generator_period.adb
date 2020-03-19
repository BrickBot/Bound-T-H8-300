-- GENERIC FUNCTION TO DETERMINE THE PERIOD OF A RANDOM NUMBER GENERATOR
   ---------------------------------------------------------------------

-- Creation : 18-AUG-1988 by Mats Weber.


function Random_Generator_Period return Count is

   X_N  : Random_Number_1;
   X_2n : Random_Number_2;

begin
   loop
      X_N  := Random_1;
      X_2n := Random_2;
      X_2n := Random_2;
      exit when Equal(X_N, X_2n);
   end loop;
   -- At this point, both X_N and X_2N have
   -- entered the random generator's loop.
   declare

      N : Count := Zero;

   begin
      loop
         X_N  := Random_1;
         X_2n := Random_2;
         X_2n := Random_2;
         N := Succ(N);
         exit when Equal(X_N, X_2n);
      end loop;
      return N;
   end;
end Random_Generator_Period;
