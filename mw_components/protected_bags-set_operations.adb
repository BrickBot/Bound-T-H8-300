-- Filename        : protected_bags-set_operations.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : Fri Jul  3 15:50:14 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Fri Jul  3 16:07:42 1998
-- Update Count    : 2


with Bags.Set_Operations;

package body Protected_Bags.Set_Operations is
------------------------------------------

   package Unprotected_Bags_Set_Operations is
      new Unprotected_Bags.Set_Operations;


   use Unprotected_Bags_Set_Operations,
       Read_Write_Semaphore,
       Implementation;


   function "=" (Left, Right : Bag) return Boolean is

      function Do_It return Boolean is
      begin
         return Left.The_Bag = Right.The_Bag;
      end;

      function Protected_Do_It is
         new Protected_Double_Function(Result_Type => Boolean,
                                       Action      => Do_It);

   begin
      return Protected_Do_It(First_Semaphore  => Left.Lock,
                             First_Operation  => Read,
                             Second_Semaphore => Right.Lock,
                             Second_Operation => Read);
   end "=";


   function "<" (Left, Right : Bag) return Boolean is

      function Do_It return Boolean is
      begin
         return Left.The_Bag < Right.The_Bag;
      end;

      function Protected_Do_It is
         new Protected_Double_Function(Result_Type => Boolean,
                                       Action      => Do_It);

   begin
      return Protected_Do_It(First_Semaphore  => Left.Lock,
                             First_Operation  => Read,
                             Second_Semaphore => Right.Lock,
                             Second_Operation => Read);
    end "<";


   function ">" (Left, Right : Bag) return Boolean is
   begin
      return Right < Left;
   end ">";


   function "<=" (Left, Right : Bag) return Boolean is

      function Do_It return Boolean is
      begin
         return Left.The_Bag <= Right.The_Bag;
      end;

      function Protected_Do_It is
         new Protected_Double_Function(Result_Type => Boolean,
                                       Action      => Do_It);

   begin
      return Protected_Do_It(First_Semaphore  => Left.Lock,
                             First_Operation  => Read,
                             Second_Semaphore => Right.Lock,
                             Second_Operation => Read);
   end "<=";


   function ">=" (Left, Right : Bag) return Boolean is
   begin
      return Right <= Left;
   end ">=";

end Protected_Bags.Set_Operations;
