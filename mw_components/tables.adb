-- GENERIC PACKAGE FOR TABLES
   --------------------------

-- Creation : 11-NOV-1985 by Mats Weber


package body Tables is
-------------------

   function Key_Of (The_Key_Item : Key_Item) return Key_Type is
   begin
      return The_Key_Item.The_Key;
   end Key_Of;


   procedure Insert (Key : in Key_Type; Item : in Item_Type; Into : in out Table) is
   begin
      Insert(Item => Key_Item'(Key, Item), Into => Into);
   exception
      when Key_Item_Bags.Duplicate_Key =>
         raise Duplicate_Key;
   end Insert;


   procedure Remove (Key : in Key_Type; From : in out Table) is
   begin
      Key_Item_Bags.Remove(Key, From => Key_Item_Bags.Bag(From));
   exception
      when Key_Item_Bags.Nonexistent_Key =>
         raise Nonexistent_Key;
   end Remove;


   procedure Remove (Key : in Key_Type; From : in out Table; Removed_Item : out Item_Type) is

      Equivalent_Key_Item : Key_Item;

   begin
      Key_Item_Bags.Remove(Key          => Key,
                           From         => Key_Item_Bags.Bag(From),
                           Removed_Item => Equivalent_Key_Item);
      Removed_Item := Equivalent_Key_Item.The_Item;
   exception
      when Key_Item_Bags.Nonexistent_Key =>
         raise Nonexistent_Key;
   end Remove;


   procedure Replace (Key : in Key_Type; New_Item : in Item_Type; Within : in out Table) is
   begin
      Key_Item_Bags.Replace(Key      => Key,
                            New_Item => Key_Item'(Key, New_Item),
                            Within   => Key_Item_Bags.Bag(Within));
   exception
      when Key_Item_Bags.Nonexistent_Key =>
         raise Nonexistent_Key;
   end Replace;


   procedure Update (Key : in Key_Type; Within : in out Table) is

      procedure Modify_Key_Item(Item : in out Key_Item) is
      begin
         Modify(Item.The_Item);
      end;

      pragma Inline(Modify_Key_Item);

      procedure Do_Update is new Key_Item_Bags.Update(Modify_Key_Item);

   begin
      Do_Update(Key, Within => Key_Item_Bags.Bag(Within));
   exception
      when Key_Item_Bags.Nonexistent_Key =>
         raise Nonexistent_Key;
   end Update;


   function Search (Key : Key_Type; Within : Table) return Item_Type is
   begin
      return Search(Key, Within).The_Item;
   exception
      when Key_Item_Bags.Nonexistent_Key =>
         raise Nonexistent_Key;
   end Search;


   function Member (Key : Key_Type; Of_Table : Table) return Boolean is
   begin
      return Key_Item_Bags.Member(Key, Key_Item_Bags.Bag(Of_Table));
   end Member;


   function Card (Of_Table : Table) return Natural_Count is
   begin
      return Key_Item_Bags.Card(Key_Item_Bags.Bag(Of_Table));
   end Card;


   function Empty (The_Table : Table) return Boolean is
   begin
      return Key_Item_Bags.Empty(Key_Item_Bags.Bag(The_Table));
   end Empty;


   function Min (Of_Table : Table) return Key_Type is
   begin
      return Min(Of_Table).The_Key;
   exception
      when Key_Item_Bags.Bag_Empty =>
         raise Table_Empty;
   end Min;


   function Max (Of_Table : Table) return Key_Type is
   begin
      return Max(Of_Table).The_Key;
   exception
      when Key_Item_Bags.Bag_Empty =>
         raise Table_Empty;
   end Max;


   function Min (Of_Table : Table) return Item_Type is
   begin
      return Min(Of_Table).The_Item;
   exception
      when Key_Item_Bags.Bag_Empty =>
         raise Table_Empty;
   end Min;


   function Max (Of_Table : Table) return Item_Type is
   begin
      return Max(Of_Table).The_Item;
   exception
      when Key_Item_Bags.Bag_Empty =>
         raise Table_Empty;
   end Max;


   procedure Assign (Object : in out Table; Value : in Table) is
   begin
      Key_Item_Bags.Assign(Key_Item_Bags.Bag(Object), Key_Item_Bags.Bag(Value));
   end Assign;


   procedure Destroy (The_Table : in out Table) is
   begin
      Key_Item_Bags.Destroy(Key_Item_Bags.Bag(The_Table));
   end Destroy;


   procedure Swap (Left, Right : in out Table) is
   begin
      Key_Item_Bags.Swap(Key_Item_Bags.Bag(Left), Key_Item_Bags.Bag(Right));
   end Swap;


   procedure Traversal (On_Table : in Table) is

      procedure Key_Item_Action (X : in Key_Item) is
      begin
         Action(X.The_Key, X.The_Item);
      end;

      pragma Inline(Key_Item_Action);

      procedure Traverse is new Key_Item_Bags.Traversal(Key_Item_Action);

   begin
      Traverse(Key_Item_Bags.Bag(On_Table));
   end Traversal;


   procedure Update_All (Within : in out Table) is

      procedure Update_Key_Item (X : in out Key_Item) is
      begin
         Update_Item(X.The_Key, X.The_Item);
      end;

      pragma Inline(Update_Key_Item);

      procedure Update_Table is new Key_Item_Bags.Update_All(Update_Key_Item);

   begin
      Update_Table(Key_Item_Bags.Bag(Within));
   end Update_All;

end Tables;
