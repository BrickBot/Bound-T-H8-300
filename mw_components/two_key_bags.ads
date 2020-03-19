-- Bags with two keys
   ------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Thu May 28 19:54:14 1998
-- Update Count    : 3

-- Creation : 24-Apr-1992 by Mats Weber.


with Bags;

generic
   type Key_1_Type (<>) is limited private;
   type Key_2_Type (<>) is limited private;
   type Item_Type is private;
   with function Key_1_Of (Item : in Item_Type) return Key_1_Type;
   with function Key_2_Of (Item : in Item_Type) return Key_2_Type;
   with function "=" (X, Y : Key_1_Type) return Boolean is <>;
   with function "=" (X, Y : Key_2_Type) return Boolean is <>;
   with function "<" (X, Y : Key_1_Type) return Boolean is <>;
   with function "<" (X, Y : Key_2_Type) return Boolean is <>;
   type Count is range <>;    -- must include 0
package Two_Key_Bags is
--------------------

   type Bag is limited private;

   subtype Natural_Count  is Count range 0 .. Count'Last;

   Nonexistent_Key_1,
   Nonexistent_Key_2,
   Duplicate_Key_1,
   Duplicate_Key_2,
   Bag_Empty         : exception;


   procedure Insert (Item : in Item_Type; Into : in out Bag);
      -- Inserts item ITEM into bag INTO
      -- if an item with the same key is already in the bag,
      -- then DUPLICATE_KEY_i is raised.

   procedure Remove (Key_1 : in Key_1_Type; From : in out Bag);
   procedure Remove (Key_1 : in Key_1_Type; From : in out Bag; Removed_Item : out Item_Type);
   procedure Remove (Key_2 : in Key_2_Type; From : in out Bag);
   procedure Remove (Key_2 : in Key_2_Type; From : in out Bag; Removed_Item : out Item_Type);
      -- Removes the first item in the bag that has a key_i
      -- equal to KEY_i. NONEXISTENT_KEY_i is raised if no such
      -- item is found.

   function Search (Key_1 : Key_1_Type; Within : Bag) return Item_Type;
   function Search (Key_2 : Key_2_Type; Within : Bag) return Item_Type;
      -- Returns the first item in FROM that has a key_i equal to KEY_i.
      -- NONEXISTENT_KEY_i will be raised if no such item is
      -- found in the bag.

   function Member (Key_1 : Key_1_Type; Of_Bag : Bag) return Boolean;
   function Member (Key_2 : Key_2_Type; Of_Bag : Bag) return Boolean;
      -- Returns TRUE if and only if an item with a key_i
      -- equal to KEY_i is in OF_BAG.


   function Min_1 (Of_Bag : Bag) return Item_Type;
   function Min_2 (Of_Bag : Bag) return Item_Type;
   function Max_1 (Of_Bag : Bag) return Item_Type;
   function Max_2 (Of_Bag : Bag) return Item_Type;
      -- Return the item with minimum/maximum key_i value
      -- inserted in OF_BAG.
      -- Raises Bag_Empty is Of_Bag is empty.


   function Empty (The_Bag : Bag) return Boolean;
      -- Tests if THE_BAG is empty.

   function Card (Of_Bag : Bag) return Natural_Count;
      -- Retuns the number of items currently in OF_BAG.


   generic
      with procedure Action (Item : in Item_Type);
   procedure Traversal_1 (The_Bag : in Bag);
      -- Traverses The_Bag in ascending key_1 order.

   generic
      with procedure Action (Item : in Item_Type);
   procedure Traversal_2 (The_Bag : in Bag);
      -- Traverses The_Bag in ascending key_2 order.


   procedure Assign (Object : in out Bag; Value : in Bag);
      -- Copies bag VALUE into OBJECT, destroying OBJECT if necessary (OBJECT := VALUE;).

   procedure Swap (Left, Right : in out Bag);
      -- Exchanges LEFT and RIGHT in an efficient way.

   procedure Destroy (The_Bag : in out Bag);
      -- Destroys THE_BAG.

private

   function "=" (Left, Right : Item_Type) return Boolean is abstract;
      -- Make sure we don't use equality on Item_Type, we want to use
      -- it only on the key types.

   package Bags_1 is new Bags(Key_Type  => Key_1_Type,
                              Item_Type => Item_Type,
                              Key_Of    => Key_1_Of,
                              Count     => Count);

   package Bags_2 is new Bags(Key_Type  => Key_2_Type,
                              Item_Type => Item_Type,
                              Key_Of    => Key_2_Of,
                              Count     => Count);

   type Bag is
      record
         Bag_1 : Bags_1.Bag(Duplicate_Keys_Allowed => False);
         Bag_2 : Bags_2.Bag(Duplicate_Keys_Allowed => False);
      end record;

end Two_Key_Bags;
