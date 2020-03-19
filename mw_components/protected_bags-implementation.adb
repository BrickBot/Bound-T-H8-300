-- Filename        : protected_bags-implementation.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : Wed Jul  1 20:11:57 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jul  1 20:17:34 1998
-- Update Count    : 1


separate (Protected_Bags)

package body Implementation is
---------------------------

   function To_Traversal_Order (Order : Traversal_Order) return Unprotected_Bags.Traversal_Order is
   begin
      case Order is
         when Ascending =>
            return Unprotected_Bags.Ascending;
         when Descending =>
            return Unprotected_Bags.Descending;
      end case;
   end To_Traversal_Order;


   procedure Protected_Action
      (The_Semaphore : in Read_Write_Semaphore.Semaphore;
       The_Operation : in Read_Write_Semaphore.Kind_Of_Operation) is
   begin
      Seize(The_Semaphore, The_Operation);
      begin
         Action;
      exception
         when others =>
            Release(The_Semaphore, The_Operation);
            raise;
      end;
      Release(The_Semaphore, The_Operation);
   exception
      when Unprotected_Bags.Nonexistent_Key =>
         raise Nonexistent_Key;
      when Unprotected_Bags.Duplicate_Key =>
         raise Duplicate_Key;
      when Unprotected_Bags.Invalid_Key =>
         raise Invalid_Key;
      when Unprotected_Bags.Bag_Empty =>
         raise Bag_Empty;
   end Protected_Action;


   procedure Protected_Double_Action
      (First_Semaphore,
       Second_Semaphore : in Read_Write_Semaphore.Semaphore;
       First_Operation,
       Second_Operation : in Read_Write_Semaphore.Kind_Of_Operation) is
   begin
      Seize(First_Semaphore, First_Operation);
      Seize(Second_Semaphore, Second_Operation);
      begin
         Action;
      exception
         when others =>
            Release(Second_Semaphore, Second_Operation);
            Release(First_Semaphore, First_Operation);
            raise;
      end;
      Release(Second_Semaphore, Second_Operation);
      Release(First_Semaphore, First_Operation);
   exception
      when Unprotected_Bags.Nonexistent_Key =>
         raise Nonexistent_Key;
      when Unprotected_Bags.Duplicate_Key =>
         raise Duplicate_Key;
      when Unprotected_Bags.Invalid_Key =>
         raise Invalid_Key;
      when Unprotected_Bags.Bag_Empty =>
         raise Bag_Empty;
   end Protected_Double_Action;


   function Protected_Function
      (The_Semaphore : in Read_Write_Semaphore.Semaphore;
       The_Operation : in Read_Write_Semaphore.Kind_Of_Operation) return Result_Type is
   begin
      Seize(The_Semaphore, The_Operation);
      begin
         declare

            Result : constant Result_Type := Action;

         begin
            Release(The_Semaphore, The_Operation);
            return Result;
         end;
      exception
         when others =>
            Release(The_Semaphore, The_Operation);
            raise;
      end;
   exception
      when Unprotected_Bags.Nonexistent_Key =>
         raise Nonexistent_Key;
      when Unprotected_Bags.Duplicate_Key =>
         raise Duplicate_Key;
      when Unprotected_Bags.Invalid_Key =>
         raise Invalid_Key;
      when Unprotected_Bags.Bag_Empty =>
         raise Bag_Empty;
   end Protected_Function;


   function Protected_Double_Function
      (First_Semaphore,
       Second_Semaphore : in Read_Write_Semaphore.Semaphore;
       First_Operation,
       Second_Operation : in Read_Write_Semaphore.Kind_Of_Operation) return Result_Type is
   begin
      Seize(First_Semaphore, First_Operation);
      Seize(Second_Semaphore, Second_Operation);
      begin
         declare

            Result : constant Result_Type := Action;

         begin
            Release(Second_Semaphore, Second_Operation);
            Release(First_Semaphore, First_Operation);
            return Result;
         end;
      exception
         when others =>
            Release(Second_Semaphore, Second_Operation);
            Release(First_Semaphore, First_Operation);
            raise;
      end;
   exception
      when Unprotected_Bags.Nonexistent_Key =>
         raise Nonexistent_Key;
      when Unprotected_Bags.Duplicate_Key =>
         raise Duplicate_Key;
      when Unprotected_Bags.Invalid_Key =>
         raise Invalid_Key;
      when Unprotected_Bags.Bag_Empty =>
         raise Bag_Empty;
   end Protected_Double_Function;

end Implementation;
