-- PACKAGE CONTAINING USEFUL DECLARATIONS FOR EXPLORING BCH CODES
   --------------------------------------------------------------

-- Creation : 21-AUG-1989 by Mats Weber.


with Number_Images,
     Text_IO;

use Text_IO;

package body Check_BCH_Bound_Common is
-----------------------------------

   function All_In_ZpZ (Elements : Vector) return Boolean is
   begin
      for I in Elements'Range loop
         if not In_ZpZ(Elements(I)) then
            return False;
         end if;
      end loop;
      return True;
   end All_In_ZpZ;


   function To_Word (Indices : Word_Indices;
                     Values  : Horla_Vector) return Word is

      Result : Word := (0 => 1) & (1..Word'Last => 0);

   begin
      for I in Horla loop
         Result(Indices(I)) := To_ZpZ(Values(I));
      end loop;
      return Result;
   end To_Word;


   procedure Put (P : in Polynomial) is

      Deg_P : constant Degree := Deg(P);

      function Image is new Number_Images.Integer_Image(ZpZ);
      function Image is new Number_Images.Integer_Image(Integer);

   begin
      if Deg_P = -1 then
         Put("0");
      else
         for I in reverse P'Range loop
            if P(I) /= 0 then
               if P(I) >= 0 then
                  if I /= Deg_P then
                     Put(" + ");
                  end if;
               else
                  if I = Deg_P then
                     Put("- ");
                  else
                     Put(" - ");
                  end if;
               end if;
               if abs P(I) /= 1 or I = 0 then
                  Put(Image(abs P(I)));
               end if;
               case I is
                  when 0 =>
                     null;
                  when 1 =>
                     Put("x");
                  when others =>
                     Put("x^" & Image(I));
               end case;
            end if;
         end loop;
      end if;
   end Put;

begin
   for J in Lambda_K'Range loop
      Lambda_K(J) := Lambda ** Integer(J);
   end loop;
end Check_BCH_Bound_Common;
