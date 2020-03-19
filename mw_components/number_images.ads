-- STRING IMAGES FOR NUMERIC TYPES
   -------------------------------

-- Revision : 29-JAN-1992 by Mats Weber, added exception Invalid_Number_String to
--                                       replace Constraint_Error.
-- Revision : 20-OCT-1988 by Mats Weber, added generic parameters for default layout
--                                       and VALUE generic functions.

-- Creation : 10-DEC-1987 by Mats Weber.


package Number_Images is
---------------------

   generic
      type Int is range <>;
      Default_Width : in Natural := 0;
   function Integer_Image (Value : Int;
                           Width : Natural := Default_Width) return String;
      -- Returns a string representation of VALUE.


   generic
      type Real is digits <>;
      Default_Fore : in Natural := 0;
      Default_Aft  : in Natural := Real'Digits - 1;
      Default_Exp  : in Natural := 0;
   function Float_Image (Value : Real;
                         Fore  : Natural := Default_Fore;
                         Aft   : Natural := Default_Aft;
                         Exp   : Natural := Default_Exp) return String;
      -- Returns a string representation of VALUE.


   generic
      type Real is delta <>;
      Default_Fore : in Natural := 0;
      Default_Aft  : in Natural := Real'Aft;
      Default_Exp  : in Natural := 0;
   function Fixed_Image (Value : Real;
                         Fore  : Natural := Default_Fore;
                         Aft   : Natural := Default_Aft;
                         Exp   : Natural := Default_Exp) return String;
      -- Returns a string representation of VALUE.


   generic
      type Int is range <>;
   function Integer_Value (Image : String) return Int;
      -- Returns IMAGE interpreted as an integer literal.
      -- Will raise INVALID_NUMBER_STRING if there is a syntax error
      -- or if the resulting value lies outside the range of INT.


   generic
      type Real is digits <>;
   function Float_Value (Image : String) return Real;
      -- Returns IMAGE interpreted as a real literal.
      -- Will raise INVALID_NUMBER_STRING if there is a syntax error
      -- or if the resulting value lies outside the range of REAL.
      -- Note that IMAGE must contain a decimal point.


   generic
      type Real is delta <>;
   function Fixed_Value (Image : String) return Real;
      -- Returns IMAGE interpreted as a real literal.
      -- Will raise INVALID_NUMBER_STRING if there is a syntax error
      -- or if the resulting value lies outside the range of REAL.
      -- Note that IMAGE must contain a decimal point.


   Invalid_Number_String : exception;

end Number_Images;
