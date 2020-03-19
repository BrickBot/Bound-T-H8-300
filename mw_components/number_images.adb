-- STRING IMAGES FOR NUMERIC TYPES
   -------------------------------

-- Creation : 10-DEC-1987 by Mats Weber.


with String_Handler,
     Text_IO;

use String_Handler;

package body Number_Images is
--------------------------

   function Integer_Image (Value : Int;
                           Width : Natural := Default_Width) return String is

      Image : constant String := Int'Image(Value);

   begin
      if Image(Image'First) = ' ' then
         return (1..Width - (Image'Length - 1) => ' ') & String_1(Image(2..Image'Last));
      else
         return (1..Width - Image'Length => ' ') & Image;
      end if;
   end Integer_Image;


   function Float_Image (Value : Real;
                         Fore  : Natural := Default_Fore;
                         Aft   : Natural := Default_Aft;
                         Exp   : Natural := Default_Exp) return String is

      package Real_Text_IO is new Text_IO.Float_IO(Real);

      Width     : Positive;
      First_Try : Boolean := True;

   begin
      if Fore > 0 then
         Width := Fore;
      else
         Width := 1;
      end if;
      if Aft > 0 then
         Width := Width + Aft + 1;
      end if;
      if Exp > 0 then
         Width := Width + Exp + 1;
      end if;
      loop
         declare

            Image : String(1..Width);

         begin
            Real_Text_IO.Put(Item => Value, To => Image, Aft => Aft, Exp => Exp);
            if First_Try then
               return Image;
            else
               return String_1(Image(Locate(' ', Image, From => Right) + 1..Image'Last));
            end if;
         exception
            when Text_IO.Layout_Error => null;
         end;
         First_Try := False;
         Width := 2 * Width;
      end loop;
   end Float_Image;


   function Fixed_Image (Value : Real;
                         Fore  : Natural := Default_Fore;
                         Aft   : Natural := Default_Aft;
                         Exp   : Natural := Default_Exp) return String is

      package Real_Text_IO is new Text_IO.Fixed_IO(Real);

      Width     : Positive;
      First_Try : Boolean := True;

   begin
      if Fore > 0 then
         Width := Fore;
      else
         Width := 1;
      end if;
      if Aft > 0 then
         Width := Width + Aft + 1;
      end if;
      if Exp > 0 then
         Width := Width + Exp + 1;
      end if;
      loop
         declare

            Image : String(1..Width);

         begin
            Real_Text_IO.Put(Item => Value, To => Image, Aft => Aft, Exp => Exp);
            if First_Try then
               return Image;
            else
               return String_1(Image(Locate(' ', Image, From => Right) + 1..Image'Last));
            end if;
         exception
            when Text_IO.Layout_Error => null;
         end;
         First_Try := False;
         Width := 2 * Width;
      end loop;
   end Fixed_Image;


   function Integer_Value (Image : String) return Int is
   begin
      return Int'Value(Image);
   exception
      when Constraint_Error =>
         raise Invalid_Number_String;
   end Integer_Value;


   function Float_Value (Image : String) return Real is

      package Real_Text_IO is new Text_IO.Float_IO(Real);

      Result : Real;
      Last   : Positive;

   begin
      Real_Text_IO.Get(From => Image, Item => Result, Last => Last);
      if Last < Image'Last and then Image(Last + 1..Image'Last) /= (Last + 1..Image'Last => ' ') then
         raise Invalid_Number_String;
      end if;
      return Result;
   exception
      when Text_IO.Data_Error =>
         raise Invalid_Number_String;
   end Float_Value;


   function Fixed_Value (Image : String) return Real is

      package Real_Text_IO is new Text_IO.Fixed_IO(Real);

      Result : Real;
      Last   : Positive;

   begin
      Real_Text_IO.Get(From => Image, Item => Result, Last => Last);
      if Last < Image'Last and then Image(Last + 1..Image'Last) /= (Last + 1..Image'Last => ' ') then
         raise Invalid_Number_String;
      end if;
      return Result;
   exception
      when Text_IO.Data_Error =>
         raise Invalid_Number_String;
   end Fixed_Value;

end Number_Images;
