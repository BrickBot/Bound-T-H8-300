-- Bags component protected by a read/write semaphore
   --------------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Fri Jul  3 15:42:45 1998
-- Update Count    : 10

-- Revision :  3-Nov-1994 by Mats Weber, added generic procedure Destruction.

-- Creation : 24-Jul-1992 by Mats Weber.


with Bags,
     Read_Write_Semaphore;

generic
   type Key_Type (<>) is limited private;
   type Item_Type is private;
   with function Key_Of (Item : in Item_Type) return Key_Type;
   with function "=" (X, Y : Key_Type) return Boolean is <>;
   with function "<" (X, Y : Key_Type) return Boolean is <>;
   type Count is range <>;    -- must include 0
package Protected_Bags is
----------------------

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


   procedure Initialize (The_Bag     : in out Bag;
                         Priority_To : in Read_Write_Semaphore.Priority_Holders :=
                                          Read_Write_Semaphore.Undefined);
      -- Must be called once and only once to start the bag's semaphore.


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

   package Unprotected_Bags is new Bags(Key_Type,
                                        Item_Type,
                                        Key_Of,
                                        Count => Count);

   type Bag (Duplicate_Keys_Allowed : Boolean) is
      record
         The_Bag : Unprotected_Bags.Bag(Duplicate_Keys_Allowed);
         Lock    : Read_Write_Semaphore.Semaphore;
      end record;

   package Implementation is
   ----------------------

      function To_Traversal_Order (Order : Traversal_Order) return Unprotected_Bags.Traversal_Order;


      generic
         with procedure Action;
      procedure Protected_Action
         (The_Semaphore : in Read_Write_Semaphore.Semaphore;
          The_Operation : in Read_Write_Semaphore.Kind_Of_Operation);


      generic
         with procedure Action;
      procedure Protected_Double_Action
         (First_Semaphore,
          Second_Semaphore : in Read_Write_Semaphore.Semaphore;
          First_Operation,
          Second_Operation : in Read_Write_Semaphore.Kind_Of_Operation);


      generic
         type Result_Type (<>) is private;
         with function Action return Result_Type;
      function Protected_Function
         (The_Semaphore : in Read_Write_Semaphore.Semaphore;
          The_Operation : in Read_Write_Semaphore.Kind_Of_Operation) return Result_Type;


      generic
         type Result_Type (<>) is private;
         with function Action return Result_Type;
      function Protected_Double_Function
         (First_Semaphore,
          Second_Semaphore : in Read_Write_Semaphore.Semaphore;
          First_Operation,
          Second_Operation : in Read_Write_Semaphore.Kind_Of_Operation) return Result_Type;


      pragma Inline(To_Traversal_Order,
                    Protected_Action,
                    Protected_Double_Action,
                    Protected_Function,
                    Protected_Double_Function);

   end Implementation;

end Protected_Bags;
