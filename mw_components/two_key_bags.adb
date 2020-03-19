-- Bags with two keys
   ------------------

-- Creation : 24-Apr-1992 by Mats Weber.


package body Two_Key_Bags is
-------------------------

   use Bags_1,
       Bags_2;


   procedure Insert (Item : in Item_Type; Into : in out Bag) is
   begin
      if Member(Key => Key_1_Of(Item), Of_Bag => Into.Bag_1) then
         raise Duplicate_Key_1;
      elsif Member(Key => Key_2_Of(Item), Of_Bag => Into.Bag_2) then
         raise Duplicate_Key_2;
      else
         Insert(Item, Into => Into.Bag_1);
         Insert(Item, Into => Into.Bag_2);
      end if;
   end Insert;


   procedure Remove (Key_1 : in Key_1_Type; From : in out Bag) is

      Junk : Item_Type;

   begin
      Remove(Key_1, From, Removed_Item => Junk);
   end Remove;


   procedure Remove (Key_1 : in Key_1_Type; From : in out Bag; Removed_Item : out Item_Type) is

      The_Removed_Item : Item_Type;

   begin
      Remove(Key          => Key_1,
             From         => From.Bag_1,
             Removed_Item => The_Removed_Item);
      Remove(Key  => Key_2_Of(The_Removed_Item),
             From => From.Bag_2);
      Removed_Item := The_Removed_Item;
   exception
      when Bags_1.Nonexistent_Key =>
         raise Nonexistent_Key_1;
   end Remove;


   procedure Remove (Key_2 : in Key_2_Type; From : in out Bag) is

      Junk : Item_Type;

   begin
      Remove(Key_2, From, Removed_Item => Junk);
   end Remove;


   procedure Remove (Key_2 : in Key_2_Type; From : in out Bag; Removed_Item : out Item_Type) is

      The_Removed_Item : Item_Type;

   begin
      Remove(Key          => Key_2,
             From         => From.Bag_2,
             Removed_Item => The_Removed_Item);
      Remove(Key  => Key_1_Of(The_Removed_Item),
             From => From.Bag_1);
      Removed_Item := The_Removed_Item;
   exception
      when Bags_2.Nonexistent_Key =>
         raise Nonexistent_Key_2;
   end Remove;


   function Search (Key_1 : Key_1_Type; Within : Bag) return Item_Type is
   begin
      return Search(Key => Key_1, Within => Within.Bag_1);
   exception
      when Bags_1.Nonexistent_Key =>
         raise Nonexistent_Key_1;
   end Search;


   function Search (Key_2 : Key_2_Type; Within : Bag) return Item_Type is
   begin
      return Search(Key => Key_2, Within => Within.Bag_2);
   exception
      when Bags_2.Nonexistent_Key =>
         raise Nonexistent_Key_2;
   end Search;


   function Member (Key_1 : Key_1_Type; Of_Bag : Bag) return Boolean is
   begin
      return Member(Key => Key_1, Of_Bag => Of_Bag.Bag_1);
   end Member;


   function Member (Key_2 : Key_2_Type; Of_Bag : Bag) return Boolean is
   begin
      return Member(Key => Key_2, Of_Bag => Of_Bag.Bag_2);
   end Member;


   function Min_1 (Of_Bag : Bag) return Item_Type is
   begin
      return Min(Of_Bag.Bag_1);
   end Min_1;

   function Min_2 (Of_Bag : Bag) return Item_Type is
   begin
      return Min(Of_Bag.Bag_2);
   end Min_2;

   function Max_1 (Of_Bag : Bag) return Item_Type is
   begin
      return Max(Of_Bag.Bag_1);
   end Max_1;

   function Max_2 (Of_Bag : Bag) return Item_Type is
   begin
      return Max(Of_Bag.Bag_2);
   end Max_2;


   function Empty (The_Bag : Bag) return Boolean is
   begin
      return Empty(The_Bag.Bag_1);
   end Empty;


   function Card (Of_Bag : Bag) return Natural_Count is
   begin
      return Card(Of_Bag.Bag_1);
   end Card;


   procedure Traversal_1 (The_Bag : in Bag) is

      procedure Traverse_1 is new Bags_1.Traversal(Action);

   begin
      Traverse_1(The_Bag.Bag_1);
   end Traversal_1;


   procedure Traversal_2 (The_Bag : in Bag) is

      procedure Traverse_2 is new Bags_2.Traversal(Action);

   begin
      Traverse_2(The_Bag.Bag_2);
   end Traversal_2;


   procedure Assign (Object : in out Bag; Value : in Bag) is
   begin
      Assign(Object => Object.Bag_1, Value => Value.Bag_1);
      Assign(Object => Object.Bag_2, Value => Value.Bag_2);
   end Assign;


   procedure Swap (Left, Right : in out Bag) is
   begin
      Swap(Left.Bag_1, Right.Bag_1);
      Swap(Left.Bag_2, Right.Bag_2);
   end Swap;


   procedure Destroy (The_Bag : in out Bag) is
   begin
      Destroy(The_Bag.Bag_1);
      Destroy(The_Bag.Bag_2);
   end Destroy;

end Two_Key_Bags;
