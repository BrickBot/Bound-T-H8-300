-- Filename        : bags-bounded_operations.ads
-- Description     :
-- Author          : Mats Weber
-- Created On      : Wed Jul  1 16:10:11 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jul  1 16:15:18 1998
-- Update Count    : 1
-- MODIFIED EXPERIMENTALLY BY N. Holsti 2003-09-02.


generic
package Bags.Bounded_Operations is
-------------------------------

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


end Bags.Bounded_Operations;
