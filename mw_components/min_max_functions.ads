-- MINIMUM AND MAXIMUM FUNCTIONS
   -----------------------------

-- Creation : 17-NOV-1989 by Mats Weber, taken from package Utilities.


package Min_Max_Functions is
-------------------------

   generic
      type Item is limited private;
      with function "<" (X, Y : Item) return Boolean is <>;
   function Minimum (X, Y : Item) return Item;

   generic
      type Item is limited private;
      with function "<" (X, Y : Item) return Boolean is <>;
   function Maximum (X, Y : Item) return Item;


   generic
      type Index is (<>);
      type Item is limited private;
      type Item_Array is array (Index range <>) of Item;
      with function "<" (X, Y : Item) return Boolean is <>;
   function Array_Minimum (Of_Items : Item_Array) return Item;

   generic
      type Index is (<>);
      type Item is limited private;
      type Item_Array is array (Index range <>) of Item;
      with function "<" (X, Y : Item) return Boolean is <>;
   function Array_Maximum (Of_Items : Item_Array) return Item;


   generic
      type Index is (<>);
      type Item is limited private;
      type Item_Array is array (Index range <>) of Item;
      with function "<" (X, Y : Item) return Boolean is <>;
   function Minimum_Index (Of_Items : Item_Array) return Index;

   generic
      type Index is (<>);
      type Item is limited private;
      type Item_Array is array (Index range <>) of Item;
      with function "<" (X, Y : Item) return Boolean is <>;
   function Maximum_Index (Of_Items : Item_Array) return Index;


   pragma Inline(Minimum, Maximum);

end Min_Max_Functions;
