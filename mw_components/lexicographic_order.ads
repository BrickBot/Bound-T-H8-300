-- Lexicographic Ordering on Arrays from Order on Components
   ---------------------------------------------------------

-- lexicographic_order_.a
-- Author          : Mats Weber
-- Created On      : Tue Nov 21 17:32:26 1995
-- Last Modified By: Mats Weber
-- Last Modified On: Tue Nov 21 17:55:59 1995
-- Update Count    : 4


generic
   type Index is (<>);
   type Item is limited private;
   type Item_Array is array (Index range <>) of Item;
   with function "=" (Left, Right : Item) return Boolean is <>;
   with function "<" (Left, Right : Item) return Boolean is <>;
package Lexicographic_Order is
---------------------------

   function "<"  (Left, Right : Item_Array) return Boolean;
   function ">"  (Left, Right : Item_Array) return Boolean;
   function "<=" (Left, Right : Item_Array) return Boolean;
   function ">=" (Left, Right : Item_Array) return Boolean;

   pragma Inline(">", ">=");

end Lexicographic_Order;
