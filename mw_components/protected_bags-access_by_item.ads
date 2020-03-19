-- Filename        : protected_bags-access_by_item.ads
-- Description     :
-- Author          : Mats Weber
-- Created On      : Wed Jul  1 20:09:38 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Fri Jul  3 15:45:22 1998
-- Update Count    : 2


generic
   with function Equal (X, Y : Item_Type) return Boolean is "=";
      -- KEY_OF(X) = KEY_OF(Y) must always be true when EQUAL(X, Y).
package Protected_Bags.Access_By_Item is
-------------------------------------

   Nonexistent_Item : exception;


   procedure Remove (Item : in Item_Type; From : in out Bag);
   procedure Remove (Item : in Item_Type; From : in out Bag; Removed_Item : out Item_Type);
      -- Removes the first item in the bag that is EQUAL to ITEM.
      -- NONEXISTENT_ITEM is raised if no such item is found.


   function Search (Item : Item_Type; Within : Bag) return Item_Type;
      -- Returns the first item in FROM that is EQUAL to ITEM.
      -- NONEXISTENT_ITEM will be raised if no such item is
      -- found in the bag.

   function Search (Item : Item_Type; Within : Bag) return List;
      -- Returns all items in FROM that are EQUAL to ITEM.
      -- The null array will be returned if no such item is
      -- found in the bag.

   generic
      with procedure Modify (Item : in out Item_Type);
   procedure Update (Item : in Item_Type; Within : in out Bag);
      -- Updates the first item in the bag that is EQUAL to ITEM.
      -- If an attempt is made to replace ITEM with an item
      -- that has a different key, then INVALID_KEY is
      -- raised and the bag is not modified.
      -- NONEXISTENT_ITEM will be raised if no item EQUAL to ITEM is found.

   procedure Replace (Item     : in Item_Type;
                      New_Item : in Item_Type;
                      Within   : in out Bag);
      -- The same as above, the first item EQUAL to ITEM
      -- being replaced with NEW_ITEM.

   function Member (Item : Item_Type; Of_Bag : Bag) return Boolean;
      -- Returns TRUE if and only if an item EQUAL to ITEM is in OF_BAG.

end Protected_Bags.Access_By_Item;
