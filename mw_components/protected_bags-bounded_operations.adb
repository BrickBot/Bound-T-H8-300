-- Filename        : protected_bags-bounded_operations.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : Fri Jul  3 15:51:45 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Fri Jul  3 16:06:12 1998
-- Update Count    : 10


with Bags.Bounded_Operations;

package body Protected_Bags.Bounded_Operations is
----------------------------------------------

   package Unprotected_Bags_Bounded_Operations is
      new Unprotected_Bags.Bounded_Operations;


   use Unprotected_Bags_Bounded_Operations,
       Read_Write_Semaphore,
       Implementation;


   procedure Bounded_Traversal (On_Bag      : in Bag;
                                First, Last : in Key_Type;
                                Order       : in Traversal_Order := Ascending) is

      procedure Do_It is

         procedure Traverse is
            new Unprotected_Bags_Bounded_Operations.Bounded_Traversal(Action);

      begin
         Traverse(On_Bag.The_Bag, First, Last, To_Traversal_Order(Order));
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => On_Bag.Lock,
                      The_Operation => Read);
   end Bounded_Traversal;


   procedure Lower_Bounded_Traversal (On_Bag : in Bag;
                                      First  : in Key_Type;
                                      Order  : in Traversal_Order := Ascending) is

      procedure Do_It is

         procedure Traverse is
            new Unprotected_Bags_Bounded_Operations.Lower_Bounded_Traversal(Action);

      begin
         Traverse(On_Bag.The_Bag, First, To_Traversal_Order(Order));
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => On_Bag.Lock,
                      The_Operation => Read);
   end Lower_Bounded_Traversal;


   procedure Upper_Bounded_Traversal (On_Bag : in Bag;
                                      Last   : in Key_Type;
                                      Order  : in Traversal_Order := Ascending) is

      procedure Do_It is

         procedure Traverse is
            new Unprotected_Bags_Bounded_Operations.Upper_Bounded_Traversal(Action);

      begin
         Traverse(On_Bag.The_Bag, Last, To_Traversal_Order(Order));
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => On_Bag.Lock,
                      The_Operation => Read);
   end Upper_Bounded_Traversal;


   procedure Bounded_Update_All (Within      : in out Bag;
                                 First, Last : in Key_Type;
                                 Order       : in Traversal_Order := Ascending) is

      procedure Do_It is

         procedure Update is
            new Unprotected_Bags_Bounded_Operations.Bounded_Update_All(Modify);

      begin
         Update(Within.The_Bag, First, Last, To_Traversal_Order(Order));
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Within.Lock,
                      The_Operation => Write);
   end Bounded_Update_All;


   procedure Lower_Bounded_Update_All (Within : in out Bag;
                                       First  : in Key_Type;
                                       Order  : in Traversal_Order := Ascending) is

      procedure Do_It is

         procedure Update is
            new Unprotected_Bags_Bounded_Operations.Lower_Bounded_Update_All(Modify);

      begin
         Update(Within.The_Bag, First, To_Traversal_Order(Order));
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Within.Lock,
                      The_Operation => Write);
   end Lower_Bounded_Update_All;


   procedure Upper_Bounded_Update_All (Within : in out Bag;
                                       Last   : in Key_Type;
                                       Order  : in Traversal_Order := Ascending) is

      procedure Do_It is

         procedure Update is
            new Unprotected_Bags_Bounded_Operations.Upper_Bounded_Update_All(Modify);

      begin
         Update(Within.The_Bag, Last, To_Traversal_Order(Order));
      end;

      procedure Protected_Do_It is
         new Protected_Action(Action => Do_It);

   begin
      Protected_Do_It(The_Semaphore => Within.Lock,
                      The_Operation => Write);
   end Upper_Bounded_Update_All;


   function Bounded_List (From        : Bag;
                          First, Last : Key_Type;
                          Order       : Traversal_Order := Ascending) return List is

      function Do_It return List is
      begin
         return List(Bounded_List(From.The_Bag, First, Last, To_Traversal_Order(Order)));
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => List,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => From.Lock,
                             The_Operation => Read);
   end Bounded_List;


   function Lower_Bounded_List (From  : Bag;
                                First : Key_Type;
                                Order : Traversal_Order := Ascending) return List is

      function Do_It return List is
      begin
         return List(Lower_Bounded_List(From.The_Bag, First, To_Traversal_Order(Order)));
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => List,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => From.Lock,
                             The_Operation => Read);
   end Lower_Bounded_List;


   function Upper_Bounded_List (From  : Bag;
                                Last  : Key_Type;
                                Order : Traversal_Order := Ascending) return List is

      function Do_It return List is
      begin
         return List(Upper_Bounded_List(From.The_Bag, Last, To_Traversal_Order(Order)));
      end;

      function Protected_Do_It is
         new Protected_Function(Result_Type => List,
                                Action      => Do_It);

   begin
      return Protected_Do_It(The_Semaphore => From.Lock,
                             The_Operation => Read);
   end Upper_Bounded_List;

end Protected_Bags.Bounded_Operations;
