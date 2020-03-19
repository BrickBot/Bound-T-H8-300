with ZpZ_Field_Exceptions,
     Square_Root_Functions,
     Prime_Numbers;

separate (ZpZ_Field)

procedure Check_Instantiation is
-----------------------------

   function To_Integer (X : ZpZ_Integer) return Integer is
   begin
      return Integer(X);
   end;

   function Sqrt is new Square_Root_Functions.Integer_Sqrt(Int => ZpZ_Integer);

   package ZpZ_Integer_Primes is new Prime_Numbers(Int  => ZpZ_Integer,
                                                   Zero => 0,
                                                   One  => 1);

begin
   if not ZpZ_Integer_Primes.Prime(P) then
      raise ZpZ_Field_Exceptions.P_Not_Prime;
   end if;
end Check_Instantiation;
