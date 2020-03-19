-- TRUNCATION, FLOOR AND CEILING FUNCTIONS
   ---------------------------------------

-- Creation : 17-NOV-1989 by Mats Weber.


package body Truncation_Functions is
---------------------------------

   function Float_Floor (X : Real) return Int is

      X_Rounded : constant Int := Int(X);

   begin
      if Real(X_Rounded) <= X then
         return X_Rounded;
      else
         return X_Rounded - 1;
      end if;
   end Float_Floor;


   function Float_Ceiling (X : Real) return Int is

      X_Rounded : constant Int := Int(X);

   begin
      if Real(X_Rounded) >= X then
         return X_Rounded;
      else
         return X_Rounded + 1;
      end if;
   end Float_Ceiling;


   function Fixed_Floor (X : Real) return Int is

      X_Rounded : constant Int := Int(X);

   begin
      if Real(X_Rounded) <= X then
         return X_Rounded;
      else
         return X_Rounded - 1;
      end if;
   end Fixed_Floor;


   function Fixed_Ceiling (X : Real) return Int is

      X_Rounded : constant Int := Int(X);

   begin
      if Real(X_Rounded) >= X then
         return X_Rounded;
      else
         return X_Rounded + 1;
      end if;
   end Fixed_Ceiling;

end Truncation_Functions;
