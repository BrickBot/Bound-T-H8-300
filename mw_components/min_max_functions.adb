-- MINIMUM AND MAXIMUM FUNCTIONS
   -----------------------------

-- Creation : 17-NOV-1989 by Mats Weber.


package body Min_Max_Functions is
------------------------------

   function Minimum (X, Y : Item) return Item is
   begin
      if X < Y then
         return X;
      else
         return Y;
      end if;
   end Minimum;


   function Maximum (X, Y : Item) return Item is
   begin
      if X < Y then
         return Y;
      else
         return X;
      end if;
   end Maximum;


   function Array_Minimum (Of_Items : Item_Array) return Item is

      function Min is new Minimum_Index (Index, Item, Item_Array);

   begin
      return Of_Items(Min(Of_Items));
   end Array_Minimum;


   function Array_Maximum (Of_Items : Item_Array) return Item is

      function Max is new Maximum_Index (Index, Item, Item_Array);

   begin
      return Of_Items(Max(Of_Items));
   end Array_Maximum;


   function Minimum_Index (Of_Items : Item_Array) return Index is

      Minimum : Index := Of_Items'First;

   begin
      if Of_Items'First < Of_Items'Last then
         for I in Index'Succ(Of_Items'First)..Of_Items'Last loop
            if Of_Items(I) < Of_Items(Minimum) then
               Minimum := I;
            end if;
         end loop;
      end if;
      return Minimum;
   end Minimum_Index;


   function Maximum_Index (Of_Items : Item_Array) return Index is

      Maximum : Index := Of_Items'First;

   begin
      if Of_Items'First < Of_Items'Last then
         for I in Index'Succ(Of_Items'First)..Of_Items'Last loop
            if Of_Items(Maximum) < Of_Items(I) then
               Maximum := I;
            end if;
         end loop;
      end if;
      return Maximum;
   end Maximum_Index;

end Min_Max_Functions;
