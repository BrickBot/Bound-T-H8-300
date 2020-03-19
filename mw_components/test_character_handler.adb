-- Filename        : test_character_handler.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : long ago
-- Last Modified By: Mats Weber
-- Last Modified On: Tue May  5 19:52:45 1998
-- Update Count    : 5


with Character_Handler,
     Text_IO,
     Integer_Text_IO,
     User_Interface;

use Character_Handler,
    Text_IO,
    Integer_Text_IO;

procedure Test_Character_Handler is

   ASCII_Codes : constant Boolean :=
                 User_Interface.Yes_No_Answer("Display ASCII codes for all characters ? ");


   procedure Put (Ch : in Character) is
   begin
      if not ASCII_Codes and Is_Graphic(Ch) then
         Text_IO.Put(Ch);
      else
         Text_IO.Put('{');
         Put(Character'Pos(Ch), Width => 0);
         Text_IO.Put('}');
      end if;
   end Put;

begin
   Put_Line("Control Characters:");
   Put("   ");
   for Ch in Character loop
      if Is_Control(Ch) then
         Put(Ch);
      end if;
   end loop;
   New_Line;
   Put_Line("Graphic Characters:");
   Put("   ");
   for Ch in Character loop
      if Is_Graphic(Ch) then
         Put(Ch);
      end if;
   end loop;
   New_Line;
   Put_Line("Upper Case Characters:");
   Put("   ");
   for Ch in Character loop
      if Is_Upper_Case(Ch) then
         Put(Ch);
      end if;
   end loop;
   New_Line;
   Put_Line("Lower Case Characters:");
   Put("   ");
   for Ch in Character loop
      if Is_Lower_Case(Ch) then
         Put(Ch);
      end if;
   end loop;
   New_Line;
   Put_Line("Digits:");
   Put("   ");
   for Ch in Character loop
      if Is_Digit(Ch) then
         Put(Ch);
      end if;
   end loop;
   New_Line;
   Put_Line("Letters:");
   Put("   ");
   for Ch in Character loop
      if Is_Letter(Ch) then
         Put(Ch);
      end if;
   end loop;
   New_Line;
   Put_Line("Symbols:");
   Put("   ");
   for Ch in Character loop
      if Is_Symbol(Ch) then
         Put(Ch);
      end if;
   end loop;
   New_Line;

   declare

      Error : exception;

      Images : constant array (0..15) of Character := "0123456789ABCDEF";

      Values : constant array (Character) of Integer :=
               (Character'First..Character'Pred('0') => Integer'First) &
               (0, 1, 2, 3, 4, 5, 6, 7, 8, 9) &
               (Character'Succ('9') .. Character'Pred('A') => Integer'First) &
               (10, 11, 12, 13, 14, 15) &
               (Character'Succ('F') .. Character'Pred('a') => Integer'First) &
               (10, 11, 12, 13, 14, 15) &
               (Character'Succ('f') .. Character'Last => Integer'First);

   begin
      for N in 0..15 loop
         if Digit_Image(N) /= Images(N) then
            raise Error;
         end if;
      end loop;
      for Ch in Character loop
         case Ch is
            when '0'..'9' | 'A'..'F' | 'a'..'f' =>
               begin
                  if Digit_Value(Ch) /= Values(Ch) then
                     raise Error;
                  end if;
               exception
                  when Character_Handler.Not_A_Digit =>
                     raise Error;
               end;
            when others =>
               begin
                  if Digit_Value(Ch) = 0 then
                     null;
                  end if;
                  raise Error;
               exception
                  when Character_Handler.Not_A_Digit =>
                     null;
               end;
         end case;
      end loop;
   exception
      when Error =>
         New_Line;
         Put_Line("Error detected in digit functions");
   end;
end Test_Character_Handler;
