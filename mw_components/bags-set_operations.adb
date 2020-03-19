-- Filename        : bags-set_operations.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : Wed Jul  1 16:52:19 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jul  1 16:55:04 1998
-- Update Count    : 1


package body Bags.Set_Operations is
--------------------------------

   function "=" (Left, Right : Bag) return Boolean is
   begin
      return Left <= Right and then Right <= Left;
   end "=";


   function "<" (Left, Right : Bag) return Boolean is

      function Subtree_Not_In_Left (L : Link) return Boolean is
      begin
         return L /= null and then
                (not Member(Key_Of(L.Val), Of_Bag => Left) or else
                 Subtree_Not_In_Left(L.Left) or else
                 Subtree_Not_In_Left(L.Right));
      end;

   begin
      return Left <= Right and then Subtree_Not_In_Left(Right.Root);
   end "<";


   function ">" (Left, Right : Bag) return Boolean is
   begin
      return Right < Left;
   end ">";


   function "<=" (Left, Right : Bag) return Boolean is

      function Subtree_In_Right (L : Link) return Boolean is
      begin
         return L = null or else
                (Member(Key_Of(L.Val), Of_Bag => Right) and then
                 Subtree_In_Right(L.Left) and then
                 Subtree_In_Right(L.Right));
      end;

   begin
      return Subtree_In_Right(Left.Root);
   end "<=";


   function ">=" (Left, Right : Bag) return Boolean is
   begin
      return Right <= Left;
   end ">=";

end Bags.Set_Operations;
