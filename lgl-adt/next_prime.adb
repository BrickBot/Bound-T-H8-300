
with Ada.Numerics.Generic_Elementary_Functions;

function Next_Prime
      ( Minimum : in     Positive)
   return Positive is

   Current : Positive :=  2*( Minimum / 2 ) + 1;

   package My_Float is new
      Ada.Numerics.Generic_Elementary_Functions( Float );
   use My_Float;

   function Prime ( Nombre : in    Positive )
      return Boolean is
   begin
      for I in 3..abs ( Positive( Sqrt( Float( Current )))) loop
         if ( Nombre mod I = 0 ) then
            return False;
         end if;
      end loop;
      return True;
   end Prime;

begin
   loop
      if Prime( Current ) then
         return( Current );
      end if;
      Current := Current + 2;
   end loop;
end Next_Prime;
