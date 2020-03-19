with Character_Handler,
     String_Handler;

separate (Large_Integers)

function Image (X : Large_Integer) return String is
begin
   if Is_Zero(X) then
      return "0";
   else
      declare

         function To_Large_Integer is new Variable_Length_Operations.Integer_To_Large_Integer(Natural);
         function To_Digit_Number  is new Large_Integer_To_Integer(Character_Handler.Digit_Number);

         use Variable_Length_Operations;

         X_Length  : constant Slice_Index := Effective_Length(X);
         Result    : String(1..(Natural(X_Length) + 1) * (Natural_Slice_Number'Width - 1) + 1);
         Ten       : constant Large_Integer := To_Large_Integer(10);
         N         : Natural := 0;
         Y         : Large_Integer(X_Length);

      begin
         Assign(Y, Value => abs X);
         while not Is_Zero(Y) loop
            Result(Result'Last - N) := Character_Handler.Digit_Image(To_Digit_Number(Y mod Ten));
            Assign(Y, Value => Y / Ten);
            N := N + 1;
         end loop;
         if X.Sign < 0 then
            Result(Result'Last - N) := '-';
            N := N + 1;
         end if;
         return String_Handler.String_1(Result(Result'Last - N + 1..Result'Last));
      end;
   end if;
end Image;
