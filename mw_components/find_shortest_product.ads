-- GENERIC FUNCTION FOR FINDING THE SHORTEST POSSIBLE PRODUCT SATISFYING A CONDITION
   ---------------------------------------------------------------------------------

-- Creation : 22-APR-1989 by Mats Weber.


generic
   type Item is limited private;
   type Table_Index is (<>);
   type Item_Table is array (Table_Index range <>) of Item;
   type List_Index is range <>;
   type Table_Index_List is array (List_Index range <>) of Table_Index;
   with function "=" (X, Y : Item) return Boolean is <>;
   with function "*" (X, Y : Item) return Item is <>;
   with function Condition (X : Item) return Boolean;
function Find_Shortest_Product (Among : Item_Table) return Table_Index_List;
------------------------------
