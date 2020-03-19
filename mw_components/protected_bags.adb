-- Bags component protected by a read/write semaphore
   --------------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Fri Jul  3 15:53:41 1998
-- Update Count    : 22

-- Creation : 24-Jul-1992 by Mats Weber.


package body Protected_Bags is
---------------------------

   use Unprotected_Bags,
       Read_Write_Semaphore,
       Implementation;


   procedure Initialize (The_Bag     : in out Bag;
                         Priority_To : in Read_Write_Semaphore.Priority_Holders :=
                                          Read_Write_Semaphore.Undefined) is
   begin
      Initialize(The_Bag.Lock, Priority_To);
   end;


   procedure Insert (Item : in Item_Type; Into : in out Bag) is

      procedure Do_It is
      begin
         Insert(Item, Into.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Into.Lock,
                      The_Operation => Write);
   end Insert;


   procedure Insert (Items : in List; Into : in out Bag) is

      procedure Do_It is
      begin
         Insert(Unprotected_Bags.List(Items), Into.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Into.Lock,
                      The_Operation => Write);
   end Insert;


   procedure Insert (Items : in Bag; Into : in out Bag) is

      procedure Do_It is
      begin
         Insert(Items.The_Bag, Into.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Double_Action(Action => Do_It);

   begin
      Protected_Do_It(First_Semaphore  => Into.Lock,
                      First_Operation  => Write,
                      Second_Semaphore => Items.Lock,
                      Second_Operation => Read);
   end Insert;


   procedure Remove (Key : in Key_Type; From : in out Bag) is

      Junk : Item_Type;

   begin
      Remove(Key, From, Removed_Item => Junk);
   end Remove;


   procedure Remove (Key : in Key_Type; From : in out Bag; Removed_Item : out Item_Type) is

      procedure Do_It is
      begin
         Remove(Key, From.The_Bag, Removed_Item);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => From.Lock,
                      The_Operation => Write);
   end Remove;


   function Search (Key : Key_Type; Within : Bag) return Item_Type is

      function Do_It return Item_Type is
      begin
         return Search(Key, Within.The_Bag);
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => Item_Type,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => Within.Lock,
                             The_Operation => Read);
   end Search;


   function Search (Key : Key_Type; Within : Bag) return List is

      function Do_It return List is
      begin
         return List(Unprotected_Bags.List'(Search(Key, Within.The_Bag)));
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => List,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => Within.Lock,
                             The_Operation => Read);
   end Search;


   procedure Update (Key : in Key_Type; Within : in out Bag) is

      procedure Do_It is

         procedure Unprotected_Update is new Unprotected_Bags.Update(Modify);

      begin
         Unprotected_Update(Key, Within.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Within.Lock,
                      The_Operation => Write);
   end Update;


   procedure Replace (Key      : in Key_Type;
                      New_Item : in Item_Type;
                      Within   : in out Bag) is

      procedure Do_It is
      begin
         Replace(Key, New_Item, Within.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Within.Lock,
                      The_Operation => Write);
   end Replace;


   function Empty (The_Bag : Bag) return Boolean is

      function Do_It return Boolean is
      begin
         return Empty(The_Bag.The_Bag);
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => Boolean,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => The_Bag.Lock,
                             The_Operation => Read);
   end Empty;


   function Card (Of_Bag : Bag) return Natural_Count is

      function Do_It return Natural_Count is
      begin
         return Card(Of_Bag.The_Bag);
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => Natural_Count,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => Of_Bag.Lock,
                             The_Operation => Read);
   end Card;


   function Member (Key : Key_Type; Of_Bag : Bag) return Boolean is

      function Do_It return Boolean is
      begin
         return Member(Key, Of_Bag.The_Bag);
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => Boolean,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => Of_Bag.Lock,
                             The_Operation => Read);
   end Member;


   function Min (Of_Bag : Bag) return Item_Type is

      function Do_It return Item_Type is
      begin
         return Min(Of_Bag.The_Bag);
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => Item_Type,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => Of_Bag.Lock,
                             The_Operation => Read);
   end Min;


   function Max (Of_Bag : Bag) return Item_Type is

      function Do_It return Item_Type is
      begin
         return Max(Of_Bag.The_Bag);
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => Item_Type,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => Of_Bag.Lock,
                             The_Operation => Read);
   end Max;


   procedure Remove_Min (From : in out Bag) is

      Junk : Item_Type;

   begin
      Remove_Min(From, Min => Junk);
   end Remove_Min;


   procedure Remove_Min (From : in out Bag; Min : out Item_Type) is

      procedure Do_It is
      begin
         Remove_Min(From.The_Bag, Min);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action  => Do_It);

   begin
      Protected_Do_It(The_Semaphore => From.Lock,
                      The_Operation => Write);
   end Remove_Min;


   procedure Remove_Max (From : in out Bag) is

      Junk : Item_Type;

   begin
      Remove_Max(From, Max => Junk);
   end Remove_Max;


   procedure Remove_Max (From : in out Bag; Max : out Item_Type) is

      procedure Do_It is
      begin
         Remove_Max(From.The_Bag, Max);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action  => Do_It);

   begin
      Protected_Do_It(The_Semaphore => From.Lock,
                      The_Operation => Write);
   end Remove_Max;


   procedure Traversal (On_Bag : in Bag;
                        Order  : in Traversal_Order := Ascending) is

      procedure Do_It is

         procedure Traverse is new Unprotected_Bags.Traversal(Action);

      begin
         Traverse(On_Bag.The_Bag, To_Traversal_Order(Order));
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => On_Bag.Lock,
                      The_Operation => Read);
   end Traversal;


   procedure Update_All (Within : in out Bag;
                         Order  : in Traversal_Order := Ascending) is

      procedure Do_It is

         procedure Update is new Unprotected_Bags.Update_All(Modify);

      begin
         Update(Within.The_Bag, To_Traversal_Order(Order));
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Within.Lock,
                      The_Operation => Write);
   end Update_All;


   procedure Assign (Object : in out Bag; Value : in Bag) is

      procedure Do_It is
      begin
         Assign(Object.The_Bag, Value.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Double_Action(Action => Do_It);

   begin
      Protected_Do_It(First_Semaphore  => Object.Lock,
                      First_Operation  => Write,
                      Second_Semaphore => Value.Lock,
                      Second_Operation => Read);
   end Assign;


   procedure Assign (Object : in out Bag; Value : in List) is

      procedure Do_It is
      begin
         Assign(Object.The_Bag, Unprotected_Bags.List(Value));
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Object.Lock,
                      The_Operation => Write);
   end Assign;


   procedure Swap (Left, Right : in out Bag) is

      procedure Do_It is
      begin
         Swap(Left.The_Bag, Right.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Double_Action(Action => Do_It);

   begin
      Protected_Do_It(First_Semaphore  => Left.Lock,
                      First_Operation  => Write,
                      Second_Semaphore => Right.Lock,
                      Second_Operation => Write);
   end Swap;


   procedure Destroy (The_Bag : in out Bag) is

      procedure Do_It is
      begin
         Destroy(The_Bag.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => The_Bag.Lock,
                      The_Operation => Write);
   end Destroy;


   procedure Destruction (The_Bag : in out Bag) is

      procedure Do_It is

         procedure Do_Destroy is new Unprotected_Bags.Destruction(Destroy);

      begin
         Do_Destroy(The_Bag.The_Bag);
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => The_Bag.Lock,
                      The_Operation => Write);
   end Destruction;


   function To_List (From  : Bag;
                     Order : Traversal_Order := Ascending) return List is

      function Do_It return List is
      begin
         return List(To_List(From.The_Bag, To_Traversal_Order(Order)));
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => List,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => From.Lock,
                             The_Operation => Read);
   end To_List;


   package body Implementation is separate;

end Protected_Bags;
