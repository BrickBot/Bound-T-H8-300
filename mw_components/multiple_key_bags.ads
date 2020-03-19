-- GENERIC BAGS WITH MULTIPLE KEYS
   -------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jun  8 11:45:46 1998
-- Update Count    : 6

-- Creation : 21-DEC-1988 by Mats Weber.


with Keys,
     Tables;

generic
   type Item_Type is private;
   type Count is range <>;
   Number_Of_Keys : in Natural;
package Multiple_Key_Bags is
-------------------------

   type Bag is limited private;
   --------

   subtype Natural_Count  is Count range 0 .. Count'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;

   type List is array (Positive_Count range <>) of Item_Type;

   Nonexistent_Key,
   Bag_Empty,
   Invalid_Instantiation : exception;


   procedure Insert (Item : in Item_Type; Into : in out Bag);
      -- Adds ITEM to INTO.

   generic
      type Key_Type is private;
      with function Key_Of (Item : Item_Type) return Key_Type;
      with function "<" (Left, Right : Key_Type) return Boolean is <>;
   package Access_By_Key is
   ---------------------

      -- This package must be instantiated exactly NUMBER_OF_KEYS times;
      -- otherwise, INVALID_INSTANTIATION will be raised.

      -- Duplicate keys are always retrieved in the order in which they were
      -- inserted, i.e. bags with duplicate keys work as if insertion
      -- order were a secondary key.
      -- "The first item associated with ..." means the one that
      -- was inserted first.


      procedure Remove (Key : in Key_Type; From : in out Bag);
      procedure Remove (Key : in Key_Type; From : in out Bag; Removed_Item : out Item_Type);
         -- Removes the first item associated with KEY from FROM and
         -- returns this item in REMOVED_ITEM.
         -- Raises NONEXISTENT_KEY if KEY is not found.

      function Search (Key : Key_Type; Within : Bag) return Item_Type;
         -- Returns the first item associated with KEY in WITHIN.
         -- Raises NONEXISTENT_KEY if KEY is not found.

      function Search (Key : Key_Type; Within : Bag) return List;
         -- Returns the list of all items associated with KEY in WITHIN.
         -- Returns the null array if KEY is not found.

      function Member (Key : Key_Type; Of_Bag : Bag) return Boolean;
         -- Tests if KEY is in OF_BAG.

      function Min (Of_Bag : Bag) return Key_Type;
      function Max (Of_Bag : Bag) return Key_Type;
         -- Return the smallest (greatest) key in OF_BAG.
         -- Will raise BAG_EMPTY if OF_BAG is empty.

      function Min (Of_Bag : Bag) return Item_Type;
      function Max (Of_Bag : Bag) return Item_Type;
         -- Return the item associated with the smallest (greatest) key in OF_BAG.
         -- Will raise BAG_EMPTY if OF_BAG is empty.

      procedure Remove_Min (From : in out Bag);
      procedure Remove_Min (From : in out Bag; Min : out Item_Type);
         -- Returns the first (smallest) element of FROM
         -- and removes this element from the bag.
         -- Will raise BAG_EMPTY if FROM is empty.

      procedure Remove_Max (From : in out Bag);
      procedure Remove_Max (From : in out Bag; Max : out Item_Type);
         -- Returns the last (greatest) element of FROM
         -- and removes this element from the bag.
         -- Will raise BAG_EMPTY if FROM is empty.

      generic
         with procedure Action (Item : in Item_Type);
      procedure Traversal (On_Bag : in Bag);
         -- Executes ACTION for all keys in ON_BAG.

      generic
         with procedure Update_Item (Item : in out Item_Type);
      procedure Update_All (Within : in out Bag);
         -- Calls UPDATE_ITEM for all keys in WITHIN.


      pragma Inline(Member, Min, Max);

   end Access_By_Key;


   function Card (Of_Bag : Bag) return Natural_Count;
      -- Returns the number of items in OF_BAG.

   function Empty (The_Bag : Bag) return Boolean;
      -- Tests if THE_BAG is empty.

   procedure Assign (Object : in out Bag; Value : in Bag);
      -- Copies VALUE into OBJECT (OBJECT := VALUE).

   procedure Destroy (The_Bag : in out Bag);
      -- Destroys THE_BAG.

   procedure Swap (Left, Right : in out Bag);
      -- Exchanges LEFT and RIGHT.

   procedure Finalize;
      -- Must be called before exiting the block containing the instance
      -- of MULTIPLE_KEY_BAGS.

private

   function "=" (Left, Right : Item_Type) return Boolean is abstract;
      -- Make sure we don't use equality on Item_Type, we want to use
      -- it only on the key types.

   package Bag_Keys is new Keys;

   package Bag_Tables is new Tables(Key_Type  => Bag_Keys.Key,
                                    "<"       => Bag_Keys."<",
                                    Item_Type => Item_Type,
                                    Count     => Count);

   package Bag_Names is new Keys;

   type Bag is
      record
         Contents : Bag_Tables.Table;
         Name     : Bag_Names.Key := Bag_Names.New_Key;
      end record;


   pragma Inline(Card, Empty, Assign, Destroy, Swap, Finalize);

end Multiple_Key_Bags;
