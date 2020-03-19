-- Lexicographic Ordering on Arrays from Order on Components
   ---------------------------------------------------------

-- lexicographic_order.a
-- Author          : Mats Weber
-- Created On      : Tue Nov 21 17:32:25 1995
-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jan  5 17:27:59 1998
-- Update Count    : 9


with Min_Max_Functions;

package body Lexicographic_Order is
--------------------------------

   type Int is new Integer;  -- Used as an integer representation of Index.

   function Min is new Min_Max_Functions.Minimum(Int);


   function "<" (Left, Right : Item_Array) return Boolean is
   begin
      for I in 0 .. Min(Left'Length, Right'Length) - 1 loop
         if Left(Index'Val(Index'Pos(Left'First) + I)) /=
            Right(Index'Val(Index'Pos(Right'First) + I))
         then
            return Left(Index'Val(Index'Pos(Left'First) + I)) <
                   Right(Index'Val(Index'Pos(Right'First) + I));
         end if;
      end loop;
      -- All equal in range 0 .. Min(Left'Length, Right'Length).
      return Left'Length < Right'Length;
   end "<";


   function ">" (Left, Right : Item_Array) return Boolean is
   begin
      return Right < Left;
   end ">";


   function "<=" (Left, Right : Item_Array) return Boolean is
   begin
      for I in 0 .. Min(Left'Length, Right'Length) - 1 loop
         if Left(Index'Val(Index'Pos(Left'First) + I)) /=
            Right(Index'Val(Index'Pos(Right'First) + I))
         then
            return Left(Index'Val(Index'Pos(Left'First) + I)) <
                   Right(Index'Val(Index'Pos(Right'First) + I));
         end if;
      end loop;
      -- All equal in range 0 .. Min(Left'Length, Right'Length).
      return Left'Length <= Right'Length;
   end "<=";


   function ">=" (Left, Right : Item_Array) return Boolean is
   begin
      return Right <= Left;
   end ">=";

end Lexicographic_Order;
