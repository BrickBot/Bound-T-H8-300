-- PACKAGE FOR CHARACTER HANDLING
   ------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Feb  3 12:48:55 1998
-- Update Count    : 6

-- Creation :  7-APR-1988 by Mats Weber.


package body Character_Handler is
------------------------------

   procedure Upper_Case (The_Character : in out Character) is
   begin
      The_Character := Upper_Case(The_Character);
   end;


   procedure Lower_Case (The_Character : in out Character) is
   begin
      The_Character := Lower_Case(The_Character);
   end;


   function Digit_Image (Of_Number : Digit_Number) return Character is
   begin
      case Of_Number is
         when 0 .. 9 =>
            return Character'Val(Character'Pos('0') + Of_Number);
         when 10 .. Digit_Number'Last =>
            return Character'Val(Character'Pos('A') + Of_Number - 10);
      end case;
   end Digit_Image;


   function Digit_Value (Of_Character : Character) return Digit_Number is
   begin
      case Of_Character is
         when '0' .. '9' =>
            return Character'Pos(Of_Character) - Character'Pos('0');
         when 'A' .. 'F' =>
            return 10 + Character'Pos(Of_Character) - Character'Pos('A');
         when 'a' .. 'f' =>
            return 10 + Character'Pos(Of_Character) - Character'Pos('a');
         when others =>
            raise Not_A_Digit;
      end case;
   end Digit_Value;

end Character_Handler;
