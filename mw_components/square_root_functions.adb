-- SQUARE ROOT FUNCTIONS
   ---------------------

-- Creation : 17-NOV-1989 by Mats Weber.


package body Square_Root_Functions is
----------------------------------

   function Integer_Sqrt (X : Int) return Int is

      U     : Int := X;
      New_U : Int;

   begin
      if X < 0 then
         raise Sqrt_Of_Negative_Value;
      end if;
      if X = 0 then
         return 0;
      end if;
      loop
         New_U := (U + X/U)/2;
         exit when New_U >= U;
         U := New_U;
      end loop;
      return U;
   end Integer_Sqrt;


   function Fixed_Sqrt (X : Real) return Real is

      U     : Real := X;
      New_U : Real;

   begin
      if X < 0.0 then
         raise Sqrt_Of_Negative_Value;
      end if;
      if X = 0.0 then
         return 0.0;
      end if;
      if X >= 1.0 then          -- the series is decreasing
         loop
            New_U := (U + Real(X/U))/2;
            exit when New_U >= U;
            U := New_U;
         end loop;
         return U;
      else                      -- the series is increasing
         loop
            New_U := (U + Real(X/U))/2;
            exit when New_U <= U;
            U := New_U;
         end loop;
         return New_U;
      end if;
   end Fixed_Sqrt;

end Square_Root_Functions;
