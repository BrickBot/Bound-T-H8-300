-- Filename        : protected_bags-bounded_operations.ads
-- Description     :
-- Author          : Mats Weber
-- Created On      : Fri Jul  3 15:40:19 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Fri Jul  3 15:48:42 1998
-- Update Count    : 2


generic
package Protected_Bags.Bounded_Operations is
-----------------------------------------

   generic
      with procedure Action (Item : in Item_Type);
   procedure Bounded_Traversal (On_Bag      : in Bag;
                                First, Last : in Key_Type;
                                Order       : in Traversal_Order := Ascending);
      -- Traverses bag ON_BAG in the specified order
      -- executing ACTION for each item in the bag
      -- which satisfies FIRST <= TO_KEY(ITEM) <= LAST.

   generic
      with procedure Action (Item : in Item_Type);
   procedure Lower_Bounded_Traversal (On_Bag : in Bag;
                                      First  : in Key_Type;
                                      Order  : in Traversal_Order := Ascending);
      -- Traverses bag ON_BAG in the specified order
      -- executing ACTION for each item in the bag
      -- which satisfies FIRST <= TO_KEY(ITEM).

   generic
      with procedure Action (Item : in Item_Type);
   procedure Upper_Bounded_Traversal (On_Bag : in Bag;
                                      Last   : in Key_Type;
                                      Order  : in Traversal_Order := Ascending);
      -- Traverses bag ON_BAG in the specified order
      -- executing ACTION for each item in the bag
      -- which satisfies TO_KEY(ITEM) <= LAST.


   generic
      with procedure Modify (Item : in out Item_Type);
   procedure Bounded_Update_All (Within      : in out Bag;
                                 First, Last : in Key_Type;
                                 Order       : in Traversal_Order := Ascending);
      -- Calls MODIFY for all items in WITHIN
      -- which satisfiy FIRST <= TO_KEY(ITEM) <= LAST.

   generic
      with procedure Modify (Item : in out Item_Type);
   procedure Lower_Bounded_Update_All (Within : in out Bag;
                                       First  : in Key_Type;
                                       Order  : in Traversal_Order := Ascending);
      -- Calls MODIFY for all items in WITHIN
      -- which satisfiy FIRST <= TO_KEY(ITEM).

   generic
      with procedure Modify (Item : in out Item_Type);
   procedure Upper_Bounded_Update_All (Within : in out Bag;
                                       Last   : in Key_Type;
                                       Order  : in Traversal_Order := Ascending);
      -- Calls MODIFY for all items in WITHIN
      -- which satisfiy TO_KEY(ITEM) <= LAST.


   function Bounded_List (From        : Bag;
                          First, Last : Key_Type;
                          Order       : Traversal_Order := Ascending) return List;

   function Lower_Bounded_List (From  : Bag;
                                First : Key_Type;
                                Order : Traversal_Order := Ascending) return List;

   function Upper_Bounded_List (From  : Bag;
                                Last  : Key_Type;
                                Order : Traversal_Order := Ascending) return List;
      -- Return the selected items of the bag in an ordered list.

end Protected_Bags.Bounded_Operations;
