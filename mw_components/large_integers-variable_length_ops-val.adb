with Character_Handler,
     String_Handler;

use Character_Handler,
    String_Handler;

separate (Large_Integers.Variable_Length_Operations)

function Value (Image : String) return Large_Integer is

   function Non_Blank (Ch : Character) return Boolean is
   begin
      return Ch /= ' ';
   end Non_Blank;

   function First_Non_Blank is new String_Handler.Scan(Non_Blank);

begin
   declare

      First     : Positive := First_Non_Blank(Within => Image, From => Left);
      Last      : constant Natural := First_Non_Blank(Within => Image, From => Right);

      Negative  : Boolean;

      function To_Large_Integer is new Integer_To_Large_Integer(Digit_Number);

      Ten       : constant Large_Integer := To_Large_Integer(10);


      function Val (Image : String) return Large_Integer is
      begin
         if Image'Length = 1 then
            return To_Large_Integer(Digit_Value(Image(Image'First)));
         else
            return To_Large_Integer(Digit_Value(Image(Image'Last))) +
                   Ten * Val(Image(Image'First..Image'Last - 1));
         end if;
      end Val;

   begin
      if Image(First) = '-' then
         Negative := True;
         First := First + 1;
      elsif Image(First) = '+' then
         Negative := False;
         First := First + 1;
      else
         Negative := False;
      end if;
      if First > Last then
         raise Constraint_Error;
      end if;
      for I in First..Last loop
         if not Is_Digit(Image(I)) then
            raise Constraint_Error;
         end if;
      end loop;
      if Negative then
         return -Val(Image(First..Last));
      else
         return Val(Image(First..Last));
      end if;
   end;
end Value;
