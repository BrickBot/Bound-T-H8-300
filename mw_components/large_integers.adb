-- GENERIC LARGE INTEGER HANDLING PACKAGE
   --------------------------------------

-- Creation : 24-MAY-1988 by Mats Weber.


package body Large_Integers is
---------------------------

   Slice_Modulo : constant Arithmetic_Number := Arithmetic_Number(Slice_Number'Last) + 1;


   function Effective_Length (X : Large_Integer) return Slice_Index is
   begin
      for K in reverse X.Value'Range loop
         if X.Value(K) /= 0 then
            return K;
         end if;
      end loop;
      return 0;
   end Effective_Length;

   pragma Inline(Effective_Length);


   procedure Assign (Object : out Large_Integer; Value : in Large_Integer) is
   begin
      if Object'Constrained then
         declare

            Value_Length : constant Slice_Index := Effective_Length(Value);

         begin
            if Value_Length <= Object.Length then
               Object.Sign := Value.Sign;
               Object.Value(0..Value_Length) := Value.Value(0..Value_Length);
               Object.Value(Value_Length + 1..Object.Length) := (others => 0);
            else
               raise Overflow;
            end if;
         end;
      else
         Object := Value;
      end if;
   end Assign;


   function Equal (X, Y : Large_Integer) return Boolean is
   begin
      if X.Sign = Y.Sign then
         declare

            X_Length : constant Slice_Index := Effective_Length(X);

         begin
            if X_Length = Effective_Length(Y) then
               return X.Value(0..X_Length) = Y.Value(0..X_Length);
            end if;
         end;
      end if;
      return False;
   end Equal;


   function "<" (X, Y : Large_Integer) return Boolean is

      X_Length : constant Slice_Index := Effective_Length(X);
      Y_Length : constant Slice_Index := Effective_Length(Y);

   begin
      if X.Sign /= Y.Sign then
         return X.Sign < Y.Sign;
      end if;
      if X_Length = Y_Length then
         for K in reverse 0..X_Length loop
            if X.Value(K) /= Y.Value(K) then
               if X.Sign > 0 then
                  return X.Value(K) < Y.Value(K);
               else
                  return X.Value(K) > Y.Value(K);
               end if;
            end if;
         end loop;
         return False;
      else
         if X.Sign > 0 then
            return X_Length < Y_Length;
         else
            return X_Length > Y_Length;
         end if;
      end if;
   end "<";


   function ">" (X, Y : Large_Integer) return Boolean is
   begin
      return Y < X;
   end ">";


   function "<=" (X, Y : Large_Integer) return Boolean is

      X_Length : constant Slice_Index := Effective_Length(X);
      Y_Length : constant Slice_Index := Effective_Length(Y);

   begin
      if X.Sign /= Y.Sign then
         return X.Sign < Y.Sign;
      end if;
      if X_Length = Y_Length then
         for K in reverse 0..X_Length loop
            if X.Value(K) /= Y.Value(K) then
               if X.Sign > 0 then
                  return X.Value(K) < Y.Value(K);
               else
                  return X.Value(K) > Y.Value(K);
               end if;
            end if;
         end loop;
         return True;
      else
         if X.Sign > 0 then
            return X_Length < Y_Length;
         else
            return X_Length > Y_Length;
         end if;
      end if;
   end "<=";


   function ">=" (X, Y : Large_Integer) return Boolean is
   begin
      return Y <= X;
   end ">=";


   function Is_Zero (X : Large_Integer) return Boolean is
   begin
      return X.Sign = 0;
   end Is_Zero;


   function Is_One (X : Large_Integer) return Boolean is
   begin
      return X.Sign = 1 and then Effective_Length(X) = 0 and then X.Value(1) = 1;
   end Is_One;


   function Large_Integer_To_Integer (X : Large_Integer) return Int is

      X_Length : constant Slice_Index := Effective_Length(X);

      Result   : Int := Int(X.Value(X_Length));

   begin
      for K in reverse 0..X_Length - 1 loop
         Result := Int(Slice_Modulo) * Result + Int(X.Value(K));
      end loop;
      case X.Sign is
         when -1 =>
            return -Result;
         when 0 =>
            return 0;
         when +1 =>
            return Result;
      end case;
   end Large_Integer_To_Integer;


   function Image (X : Large_Integer) return String is separate;


   package body Variable_Length_Operations is separate;
   ---------------------------------------

   package body Fixed_Length_Operations is separate;
   ------------------------------------

end Large_Integers;
