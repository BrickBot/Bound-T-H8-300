-- GENERIC QUICK SORT PROCEDURE
   ----------------------------

-- Creation : 15-JUN-1986 by Mats Weber.


generic
   type Index is (<>);
   type Item is private;
   type Vector is array (Index range <>) of Item;
   with function "<" (X, Y : Item) return Boolean is <>;
procedure Quick_Sort (Items : in out Vector);
