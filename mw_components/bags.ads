-- GENERIC BAGS PACKAGE IMPLEMENTED WITH AVL TREES
   -----------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jul  1 16:51:42 1998
-- Update Count    : 21

-- Revision : 19-May-1998 by Mats Weber, made full view of type Bag limited so that we can
--                                       depend on pass by reference.
-- Revision :  3-Nov-1994 by Mats Weber, added generic procedure Destruction.
-- Revision : 24-FEB-1989 by Mats Weber, renamed package from BINARY_TREES to BAGS and added
--                                       generic package ACCESS_BY_ITEM.
-- Revision : 22-SEP-1988 by Mats Weber, ensured that duplicate keys are always retrieved
--                                       in the order in which they are inserted
--                                       (for SEARCH, UPDATE, REPLACE and REMOVE).
-- Revision : 31-AUG-1988 by Mats Weber, added procedure SWAP and removed procedure MOVE.
-- Revision :  7-JUN-1988 by Mats Weber, made generic formal type KEY_TYPE limited private.
-- Revision :  3-SEP-1987 by Mats Weber, added discriminant DUPLICATE_KEYS_ALLOWED to type TREE,
--                                       suppressed INSERT and added generic type COUNT.
-- Revision : 26-JUN-1987 by Mats Weber, changed parameter names for clearer calls,
--                                       removed UPDATE_OR_ADD and REPLACE_OR_ADD,
--                                       added bounded traversal procedures.
-- Revision :  5-APR-1987 by Mats Weber, removed nested generic package ACCESS_BY_KEY
--                                       and made type KEY_TYPE and function KEY_OF
--                                       global generic parameters.
-- Revision :  9-JUN-1986 by Mats Weber, renamed EQUIVALENCE to ACCESS_BY_KEY.
-- Revision : 27-MAY-1986 by Mats Weber, added generic procedure UPDATE_ALL.
-- Revision : 22-NOV-1985 by Mats Weber, added procedure MOVE.
-- Revision :  4-NOV-1985 by Mats Weber, added generic formal type KEY_TYPE
--                                       to package ACCESS_BY_KEY.
-- Revision : 18-OCT-1985 by Mats Weber, added set comparison operators.
-- Revision :  3-SEP-1985 by Mats Weber, added UPDATE_OR_ADD.
-- Revision : 19-AUG-1985 by Mats Weber, use of Adelson-Velskii-Landis balanced trees,
--                                       provided both ADD and INSERT for tree insertion

-- Creation :  2-JUL-1985 by Mats Weber.

--:dbpool with GNAT.Debug_Pools;


generic
   type Key_Type (<>) is limited private;
   type Item_Type is private;
   with function Key_Of (Item : in Item_Type) return Key_Type;
   with function "=" (X, Y : Key_Type) return Boolean is <>;
   with function "<" (X, Y : Key_Type) return Boolean is <>;
   type Count is range <>;    -- must include 0
package Bags is
------------

   type Bag (Duplicate_Keys_Allowed : Boolean) is limited private;
   -------------------------------------------
      -- type of a bag of ITEMs

   subtype Natural_Count  is Count range 0 .. Count'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;

   type List is array (Positive_Count range <>) of Item_Type;

   type Traversal_Order is (Ascending, Descending);


   Nonexistent_Key,
   Duplicate_Key,
   Invalid_Key,
   Bag_Empty         : exception;


   -- Duplicate keys are always retrieved in the order in which they were
   -- inserted, i.e. bags with duplicate keys allowed work as if insertion
   -- order were a secondary key.
   -- "The first item with the same key" means the one that
   -- was inserted first.


   procedure Insert (Item : in Item_Type; Into : in out Bag);
      -- Inserts item ITEM into bag INTO
      -- if an item with the same key is already in the bag and
      -- INTO.DUPLICATE_KEYS_ALLOWED is false, then DUPLICATE_KEY
      -- is raised.

   procedure Insert (Items : in List; Into : in out Bag);
      -- Adds the components of ITEMS to bag INTO.
      -- If one of the keys in ITEMS is already in INTO and
      -- INTO.DUPLICATE_KEYS_ALLOWED is false then nothing
      -- is added and DUPLICATE_KEY is raised.

   procedure Insert (Items : in Bag; Into : in out Bag);
      -- Adds the components of ITEMS to bag INTO.
      -- If one of the keys in ITEMS is already in INTO and
      -- INTO.DUPLICATE_KEYS_ALLOWED is false then nothing
      -- is added and DUPLICATE_KEY is raised.

   procedure Remove (Key : in Key_Type; From : in out Bag);
   procedure Remove (Key : in Key_Type; From : in out Bag; Removed_Item : out Item_Type);
      -- Removes the first item in the bag that has a key
      -- equal to KEY. NONEXISTENT_KEY is raised if no such
      -- item is found.


   function Search (Key : Key_Type; Within : Bag) return Item_Type;
      -- Returns the first item in FROM that has a key equal to KEY.
      -- NONEXISTENT_KEY will be raised if no such item is
      -- found in the bag.

   function Search (Key : Key_Type; Within : Bag) return List;
      -- Returns all items in FROM that have a key equal to KEY.
      -- The null array will be returned if no such item is
      -- found in the bag.


   generic
      with procedure Modify (Item : in out Item_Type);
   procedure Update (Key : in Key_Type; Within : in out Bag);
      -- Updates the first item in the bag whose key is equal to KEY.
      -- If an attempt is made to replace ITEM with an item
      -- that has a different key, then INVALID_KEY is
      -- raised and the bag is not modified.
      -- NONEXISTENT_KEY will be raised if no item with a key equal to KEY is found.

   procedure Replace (Key      : in Key_Type;
                      New_Item : in Item_Type;
                      Within   : in out Bag);
      -- The same as above, the first item that has a key equal to KEY
      -- being replaced with NEW_ITEM.


   function Empty (The_Bag : Bag) return Boolean;
      -- Tests if THE_BAG is empty.

   function Card (Of_Bag : Bag) return Natural_Count;
      -- Retuns the number of items currently in OF_BAG.

   function Member (Key : Key_Type; Of_Bag : Bag) return Boolean;
      -- Returns TRUE if and only if an item with a key equal to KEY is in OF_BAG.

   function Min (Of_Bag : Bag) return Item_Type;
      -- Returns the smallest element in OF_BAG.

   function Max (Of_Bag : Bag) return Item_Type;
      -- Returns the greatest element in OF_BAG.

   procedure Remove_Min (From : in out Bag);
   procedure Remove_Min (From : in out Bag; Min : out Item_Type);
      -- Returns the first (smallest) element of bag FROM
      -- and removes this element from the bag.

   procedure Remove_Max (From : in out Bag);
   procedure Remove_Max (From : in out Bag; Max : out Item_Type);
      -- Returns the last (greatest) element of bag FROM
      -- and removes this element from the bag.


   generic
      with procedure Action (Item : in Item_Type);
   procedure Traversal (On_Bag : in Bag;
                        Order  : in Traversal_Order := Ascending);
      -- Traverses bag ON_BAG in the specified order
      -- executing ACTION for each item in the bag.


   generic
      with procedure Modify (Item : in out Item_Type);
   procedure Update_All (Within : in out Bag;
                         Order  : in Traversal_Order := Ascending);
      -- Calls MODIFY for all items in WITHIN.


   procedure Assign (Object : in out Bag; Value : in Bag);
      -- Copies bag VALUE into OBJECT, destroying OBJECT if necessary (OBJECT := VALUE;).
      -- Raises CONSTRAINT_ERROR if VALUE.DUPLICATE_KEYS_ALLOWED and
      --                            not OBJECT.DUPLICATE_KEYS_ALLOWED.

   procedure Assign (Object : in out Bag; Value : in List);
      -- Creates a bag from a list, destroying OBJECT if necessary.
      -- Raises DUPLICATE_KEY without modifying OBJECT
      -- if not OBJECT.DUPLICATE_KEYS_ALLOWED and VALUE has duplicate keys.

   procedure Swap (Left, Right : in out Bag);
      -- Exchanges LEFT and RIGHT in an efficient way.
      -- Raises CONSTRAINT_ERROR if LEFT.DUPLICATE_KEYS_ALLOWED /= RIGHT.DUPLICATE_KEYS_ALLOWED.

   procedure Destroy (The_Bag : in out Bag);
      -- Destroys THE_BAG.

   generic
      with procedure Destroy (Item : in out Item_Type);
   procedure Destruction (The_Bag : in out Bag);
      -- Destroys THE_BAG while calling Destroy for each item in it.


   function To_List (From  : Bag;
                     Order : Traversal_Order := Ascending) return List;
      -- Return the selected items of the bag in an ordered list.

private

   function "=" (Left, Right : Item_Type) return Boolean is abstract;
      -- Make sure we don't use equality on Item_Type, we want to use
      -- it only on Key_Type.

   type Cell;

   type Link is access Cell;

   --:dbpool Link_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Link'Storage_Pool use Link_Pool;

   type Equilibrium is range -1 .. +1;

   type Cell is
      record
         Val         : Item_Type;
         Balance     : Equilibrium := 0;  -- always equal to DEPTH(RIGHT) - DEPTH(LEFT)
         Left, Right : Link;
      end record;

   type Bag (Duplicate_Keys_Allowed : Boolean) is limited
      record
         Root : Link;
         Card : Natural_Count := 0;
      end record;


   pragma Inline(Empty, Card, Member, Swap);


   package Implementation is
   ----------------------

      -- This package defines primitives that are used throughout the
      -- the various child units. Having them defined in Bags' body would
      -- be nice, but would make them invisible to other children.

      function Pointer (X : Key_Type; L : Link; First_Match : Boolean) return Link;
         -- Returns a pointer to the first cell containing
         -- an item whose key is equal to X in subtree L.
         -- Returns null if X is not found.

      procedure Dispose (L : in out Link);

      -- Primitives for balancing used by procedures that delete cells

      procedure Balance_1 (P : in out Link; Depth_Reduced : in out Boolean);

      procedure Balance_2 (P : in out Link; Depth_Reduced : in out Boolean);

      generic
         with procedure Action (X : in Item_Type);
      procedure Action_On_Link (L : in Link);

      generic
         with procedure Modify (X : in out Item_Type);
      procedure Modify_On_Link (L : in Link);


      pragma Inline(Pointer, Action_On_Link, Modify_On_Link);

   end Implementation;

end Bags;
