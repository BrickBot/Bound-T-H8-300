-- SIMPLE USER INTERACTION BASED ON TEXT_IO
   ----------------------------------------

-- Revision : 19-APR-1988 by Mats Weber, added generic functions FLOAT_ANSWER and FIXED_ANSWER,
--                                       added blank stripping parameters to STRING_ANSWER.

-- Creation :  7-DEC-1987 by Mats Weber.


package User_Interface is
----------------------

   Max_User_Input_Length : constant := 132;


   function String_Answer (Prompt                : String;
                           Null_String_Allowed   : Boolean := True;
                           Strip_Leading_Blanks,
                           Strip_Trailing_Blanks : Boolean := True) return String;
      -- The lower bound of the result is always 1.


   function Yes_No_Answer (Prompt : String) return Boolean;
      -- Accepts "Y", "N", "YES" or "NO" in upper or lower case from the user
      -- and returns TRUE if and only if the user entered "Y" or "YES".


   generic
      type Int is range <>;
   function Integer_Answer (Prompt : String) return Int;
      -- Accepts an integer number from the user.

   generic
      type Real is digits <>;
   function Float_Answer (Prompt : String) return Real;
      -- Accepts a floating point number from the user.
      -- ".0" is not required (the syntax of an integer number is accepted).

   generic
      type Real is delta <>;
   function Fixed_Answer (Prompt : String) return Real;
      -- Accepts a fixed point number from the user.
      -- ".0" is not required (the syntax of an integer number is accepted).

end User_Interface;
