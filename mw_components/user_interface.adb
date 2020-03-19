-- SIMPLE USER INTERACTION BASED ON TEXT_IO
   ----------------------------------------

-- Creation :  7-DEC-1987 by Mats Weber.


with String_Handler,
     String_Case_Conversions,
     Text_IO;

use String_Handler,
    String_Case_Conversions;

package body User_Interface is
---------------------------

   function String_Answer (Prompt                : String;
                           Null_String_Allowed   : Boolean := True;
                           Strip_Leading_Blanks,
                           Strip_Trailing_Blanks : Boolean := True) return String is

      Answer         : String(1..Max_User_Input_Length);
      Answer_Length  : Natural range 0..Answer'Last;


      function Not_A_Space (Ch : Character) return Boolean is
      begin
         return Ch /= ' ' and Ch /= ASCII.HT;
      end;

      function Scan_Space is new String_Handler.Scan(Not_A_Space);

   begin
      loop
         Text_IO.Put(Prompt);
         Text_IO.Get_Line(Answer, Last => Answer_Length);
         declare

            First : Natural range 1..Answer_Length + 1 := 1;
            Last  : Natural range 0..Answer_Length     := Answer_Length;

         begin
            if Strip_Leading_Blanks then
               First := Scan_Space(Answer(1..Answer_Length), From => Left);
            end if;
            if Strip_Trailing_Blanks then
               Last := Scan_Space(Answer(1..Answer_Length), From => Right);
            end if;
            if Null_String_Allowed or else First <= Last then
               return String_1(Answer(First..Last));
            end if;
         end;
      end loop;
   end String_Answer;


   function Yes_No_Answer (Prompt : String) return Boolean is
   begin
      loop
         declare

            Answer : constant String := Upper_Case(String_Answer(Prompt                => Prompt,
                                                                 Null_String_Allowed   => False,
                                                                 Strip_Leading_Blanks  => True,
                                                                 Strip_Trailing_Blanks => True));

         begin
            if Answer = "YES" or Answer = "Y" then
               return True;
            elsif Answer = "NO" or Answer = "N" then
               return False;
            end if;
         end;
      end loop;
   end Yes_No_Answer;


   function Integer_Answer (Prompt : String) return Int is
   begin
      loop
         begin
            return Int'Value(String_Answer(Prompt, Null_String_Allowed => False));
         exception
            when Constraint_Error => null;
         end;
      end loop;
   end Integer_Answer;


   function Float_Answer (Prompt : String) return Real is

      package Real_Text_IO is new Text_IO.Float_IO(Real);

   begin
      loop
         declare

            Answer : constant String := Upper_Case(String_Answer(Prompt                => Prompt,
                                                                 Null_String_Allowed   => False,
                                                                 Strip_Leading_Blanks  => True,
                                                                 Strip_Trailing_Blanks => True));

            Result : Real;
            Last   : Natural;

         begin
            Real_Text_IO.Get(From => Answer, Item => Result, Last => Last);
            if Last = Answer'Last then
               return Result;
            end if;
         exception
            when Text_IO.Data_Error =>
               -- retry reading with ".0" added to the user's answer
               declare

                  E_Location      : constant Natural := Locate(Pattern => 'E', Within => Answer);

                  Modified_Answer : constant String := Answer(Answer'First..E_Location - 1) &
                                                       ".0" &
                                                       Answer(E_Location..Answer'Last);

               begin
                  Real_Text_IO.Get(From => Modified_Answer, Item => Result, Last => Last);
                  if Last = Modified_Answer'Last then
                     return Result;
                  end if;
               exception
                  when Text_IO.Data_Error =>
                     null;
               end;
         end;
      end loop;
   end Float_Answer;


   function Fixed_Answer (Prompt : String) return Real is

      package Real_Text_IO is new Text_IO.Fixed_IO(Real);

   begin
      loop
         declare

            Answer : constant String := Upper_Case(String_Answer(Prompt                => Prompt,
                                                                 Null_String_Allowed   => False,
                                                                 Strip_Leading_Blanks  => True,
                                                                 Strip_Trailing_Blanks => True));

            Result : Real;
            Last   : Natural;

         begin
            Real_Text_IO.Get(From => Answer, Item => Result, Last => Last);
            if Last = Answer'Last then
               return Result;
            end if;
         exception
            when Text_IO.Data_Error =>
               -- retry reading with ".0" added to the user's answer
               declare

                  E_Location      : constant Natural := Locate(Pattern => 'E', Within => Answer);

                  Modified_Answer : constant String := Answer(Answer'First..E_Location - 1) &
                                                       ".0" &
                                                       Answer(E_Location..Answer'Last);

               begin
                  Real_Text_IO.Get(From => Modified_Answer, Item => Result, Last => Last);
                  if Last = Modified_Answer'Last then
                     return Result;
                  end if;
               exception
                  when Text_IO.Data_Error =>
                     null;
               end;
         end;
      end loop;
   end Fixed_Answer;

end User_Interface;
