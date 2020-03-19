-- GENERIC PACKAGE FOR GENERATING UNIQUE KEYS
   ------------------------------------------

-- Creation : 13-APR-1988 by Mats Weber.


with Number_Images;

package body Keys is
-----------------

   Last_Key_Value : Key_Value := Null_Key_Value;


   function New_Key return Key is
   begin
      if Last_Key_Value < Key_Value'Last then
         Last_Key_Value := Last_Key_Value + 1;
         return Key'(Value => Last_Key_Value);
      else
         raise Too_Many_Keys;
      end if;
   end New_Key;


   function "<" (Left, Right : Key) return Boolean is
   begin
      return Left.Value < Right.Value;
   end "<";


   function Is_Initialized (The_Key : Key) return Boolean is
   begin
      return The_Key.Value /= Null_Key_Value;
   end Is_Initialized;


   procedure Check (The_Key : in Key) is
   begin
      if The_Key.Value = Null_Key_Value then
         raise Uninitialized_Key;
      end if;
   end Check;


   function Image (Of_Key : Key) return String is

      function Image is new Number_Images.Integer_Image(Key_Value);

   begin
      return Image(Of_Key.Value);
   end Image;

end Keys;
