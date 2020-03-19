-- GENERIC EXPONENTIATION FUNCTIONS
   --------------------------------

-- Creation : 17-NOV-1989 by Mats Weber.


package body Exponentiation_Functions is
-------------------------------------

   function Exponentiation (X : Number; N : Exponent) return Number is

      function "**" is new Natural_Exponentiation(Number,
                                                  One,
                                                  Exponent,
                                                  "*");

   begin
      if N = 0 then
         return One;
      elsif N > 0 then
         return X**N;
      else
         return Inverse(X)**(-N);
      end if;
   end Exponentiation;


   function Natural_Exponentiation (X : Number; N : Exponent) return Number is
   begin
      if N = 0 then
         return One;
      elsif N < 0 then
         raise Negative_Exponent;
      end if;
      declare

         Result        : Number := One;
         X_Raised_To_P : Number := X;
         M             : Exponent range 0..N := N;

      begin
         loop
            if M mod 2 /= 0 then
               Result := Result * X_Raised_To_P;
            end if;
            M := M / 2;
            exit when M = 0;
            X_Raised_To_P := X_Raised_To_P * X_Raised_To_P;
         end loop;
         return Result;
      end;
   end Natural_Exponentiation;

end Exponentiation_Functions;
