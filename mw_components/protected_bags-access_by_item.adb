-- Bags component protected by a read/write semaphore
   --------------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Fri Jul  3 19:05:57 1998
-- Update Count    : 8

-- Creation : 24-Jul-1992 by Mats Weber.


with Bags.Access_By_Item;

package body Protected_Bags.Access_By_Item is
------------------------------------------

   package Unprotected_Access_By_Item is
      new Unprotected_Bags.Access_By_Item(Equal);


   use Unprotected_Access_By_Item,
       Read_Write_Semaphore,
       Implementation;


   procedure Remove (Item : in Item_Type; From : in out Bag) is

      Junk : Item_Type;

   begin
      Remove(Item, From, Removed_Item => Junk);
   end Remove;

   procedure Remove (Item : in Item_Type; From : in out Bag; Removed_Item : out Item_Type) is

      procedure Do_It is
      begin
         Remove(Item, From.The_Bag, Removed_Item);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => From.Lock,
                      The_Operation => Write);
   exception
      when Unprotected_Access_By_Item.Nonexistent_Item =>
         raise Nonexistent_Item;
   end Remove;


   function Search (Item : Item_Type; Within : Bag) return Item_Type is

      function Do_It return Item_Type is
      begin
         return Search(Item, Within.The_Bag);
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => Item_Type,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => Within.Lock,
                             The_Operation => Read);
   exception
      when Unprotected_Access_By_Item.Nonexistent_Item =>
         raise Nonexistent_Item;
   end Search;

   function Search (Item : Item_Type; Within : Bag) return List is

      function Do_It return List is
      begin
         return List(Unprotected_Bags.List'(Search(Item, Within.The_Bag)));
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => List,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => Within.Lock,
                             The_Operation => Read);
   end Search;


   procedure Update (Item : in Item_Type; Within : in out Bag) is

      procedure Do_It is

         procedure Unprotected_Update is new Unprotected_Access_By_Item.Update(Modify);

      begin
         Unprotected_Update(Item, Within.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Within.Lock,
                      The_Operation => Write);
   exception
      when Unprotected_Access_By_Item.Nonexistent_Item =>
         raise Nonexistent_Item;
   end Update;


   procedure Replace (Item     : in Item_Type;
                      New_Item : in Item_Type;
                      Within   : in out Bag) is

      procedure Do_It is
      begin
         Replace(Item, New_Item, Within.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Within.Lock,
                      The_Operation => Write);
   exception
      when Unprotected_Access_By_Item.Nonexistent_Item =>
         raise Nonexistent_Item;
   end Replace;


   function Member (Item : Item_Type; Of_Bag : Bag) return Boolean is

      function Do_It return Boolean is
      begin
         return Member(Item, Of_Bag.The_Bag);
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => Boolean,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => Of_Bag.Lock,
                             The_Operation => Read);
   end Member;

end Protected_Bags.Access_By_Item;
