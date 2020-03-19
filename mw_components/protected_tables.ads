-- Tables protected by a read/write semaphore
   ------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jun  8 11:34:53 1998
-- Update Count    : 4

-- Creation : 25-Jul-1992 by Mats Weber


with Protected_Bags,
     Read_Write_Semaphore;

generic
   type Key_Type is private;
   type Item_Type is private;
   with function "<" (A, B : Key_Type) return Boolean is <>;
   type Count is range <>;
package Protected_Tables is
------------------------

   type Table is limited private;
   ----------

   subtype Natural_Count is Count range 0 .. Count'Last;

   Nonexistent_Key,
   Duplicate_Key,
   Table_Empty      : exception;


   procedure Initialize (The_Table   : in out Table;
                         Priority_To : in Read_Write_Semaphore.Priority_Holders :=
                                          Read_Write_Semaphore.Undefined);
      -- Must be called once and only once to start the table's semaphore.

   procedure Insert (Key : in Key_Type; Item : in Item_Type; Into : in out Table);
      -- Adds key KEY to table TO and associates ITEM with KEY.
      -- Raises DUPLICATE_KEY if KEY is present in TO.

   procedure Remove (Key : in Key_Type; From : in out Table);
   procedure Remove (Key : in Key_Type; From : in out Table; Removed_Item : out Item_Type);
      -- Removes key KEY from FROM and returns in REMOVED_ITEM
      -- the item that was associated with KEY.
      -- Raises NONEXISTENT_KEY if KEY is not found.

   procedure Replace (Key : in Key_Type; New_Item : in Item_Type; Within : in out Table);
      -- Associates key KEY with item NEW_ITEM; KEY must be in the table,
      -- otherwise NONEXITSTENT_KEY is raised.

   generic
      with procedure Modify (Item : in out Item_Type);
   procedure Update (Key : in Key_Type; Within : in out Table);
      -- Updates the item associated with key KEY.

   function Search (Key : Key_Type; Within : Table) return Item_Type;
      -- Returns the item associated with KEY in WITHIN.
      -- Raises NONEXISTENT_KEY if KEY is not found.

   function Member (Key : Key_Type; Of_Table : Table) return Boolean;
      -- Tests if key KEY is in OF_TABLE.

   function Card (Of_Table : Table) return Natural_Count;
      -- Returns the number of keys in OF_TABLE.

   function Empty (The_Table : Table) return Boolean;
      -- Tests if THE_TABLE is empty.

   function Min (Of_Table : Table) return Key_Type;
   function Max (Of_Table : Table) return Key_Type;
      -- Return the smallest (greatest) key in OF_TABLE.
      -- Will raise TABLE_EMPTY if OF_TABLE is empty.

   function Min (Of_Table : Table) return Item_Type;
   function Max (Of_Table : Table) return Item_Type;
      -- Return the item associated with the smallest (greatest) key in OF_TABLE.
      -- Will raise TABLE_EMPTY if OF_TABLE is empty.

   procedure Assign (Object : in out Table; Value : in Table);
      -- Copies VALUE into OBJECT (OBJECT := VALUE).

   procedure Destroy (The_Table : in out Table);
      -- Destroys THE_TABLE.

   procedure Swap (Left, Right : in out Table);
      -- Exchanges LEFT and RIGHT.

   generic
      with procedure Action (Key : in Key_Type; Item : in Item_Type);
   procedure Traversal (On_Table : in Table);
      -- Executes ACTION for all keys in ON_TABLE.

   generic
      with procedure Update_Item (Key : in Key_Type; Item : in out Item_Type);
   procedure Update_All (Within : in out Table);
      -- Calls UPDATE_ITEM for all keys in WITHIN.

private

   function "=" (Left, Right : Item_Type) return Boolean is abstract;
      -- Make sure we don't use equality on Item_Type, we want to use
      -- it only on Key_Type.

   type Key_Item is
      record
         The_Key  : Key_Type;
         The_Item : Item_Type;
      end record;

   function "=" (Left, Right : Key_Item) return Boolean is abstract;

   function Key_Of (The_Key_Item : Key_Item) return Key_Type;

   pragma Inline(Key_Of);


   package Key_Item_Bags is new Protected_Bags(Key_Type  => Key_Type,
                                               Item_Type => Key_Item,
                                               Key_Of    => Key_Of,
                                               Count     => Count);

   type Table is new Key_Item_Bags.Bag(Duplicate_Keys_Allowed => False);


   pragma Inline(Insert, Remove, Replace, Search, Member,
                 Card, Empty, Min, Max, Assign, Destroy, Swap);

end Protected_Tables;
