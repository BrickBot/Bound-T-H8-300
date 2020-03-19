-- GENERIC QUICK SORT FUNCTION
   ---------------------------

-- Created :  9-SEP-1986 by Mats Weber.

generic
  type Index is (<>);
  type Item is private;
  type Vector is array (Index range <>) of Item;
  with function "<" (X,Y : Item) return Boolean is <>;
function Quick_Sort_Function (V : Vector) return Vector;
