-- GENERIC PACKAGE FOR GENERATING UNIQUE KEYS (PROTECTED BY A HIDDEN TASK)
   -----------------------------------------------------------------------

-- Creation :  9-Jul-1992 by Mats Weber.


package body Protected_Keys is
---------------------------

   task Key_Provider is
      entry Get_New_Key (The_Key : out Key);
      entry Kill;
   end Key_Provider;


   task body Key_Provider is
   begin
      loop
         select
            accept Get_New_Key (The_Key : out Key) do
               The_Key := Key(Keys_Instance.New_Key);
            end;
         or
            accept Kill;
            exit;
         or
            terminate;
         end select;
      end loop;
   end Key_Provider;



   function New_Key return Key is

      Result : Key;

   begin
      Key_Provider.Get_New_Key(Result);
      return Result;
   exception
      when Keys_Instance.Too_Many_Keys =>
         raise Too_Many_Keys;
   end New_Key;


   function "<" (Left, Right : Key) return Boolean is
   begin
      return Keys_Instance."<"(Keys_Instance.Key(Left),
                               Keys_Instance.Key(Right));
   end "<";


   function Is_Initialized (The_Key : Key) return Boolean is
   begin
      return Keys_Instance.Is_Initialized(Keys_Instance.Key(The_Key));
   end Is_Initialized;


   procedure Check (The_Key : in Key) is
   begin
      Keys_Instance.Check(Keys_Instance.Key(The_Key));
   exception
      when Keys_Instance.Uninitialized_Key =>
         raise Uninitialized_Key;
   end Check;


   function Image (Of_Key : Key) return String is
   begin
      return Keys_Instance.Image(Keys_Instance.Key(Of_Key));
   end Image;


   procedure Kill is
   begin
      Key_Provider.Kill;
   end;

end Protected_Keys;
