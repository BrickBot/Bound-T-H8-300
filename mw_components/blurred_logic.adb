-- Blurred Logic (False, Maybe, True).
   -----------------------------------

-- blurred_logic.a
-- Author          : Mats Weber
-- Created On      : Tue Jun 11 14:27:04 1996
-- Last Modified By: Mats Weber
-- Last Modified On: Tue Jun 11 14:35:56 1996
-- Update Count    : 2


package body Blurred_Logic is
--------------------------

   Not_Table : constant array (Boolean_Maybe) of Boolean_Maybe :=
               (True, Maybe, False);

   And_Table : constant array (Boolean_Maybe, Boolean_Maybe) of Boolean_Maybe :=
               ((False, False, False),
                (False, Maybe, Maybe),
                (False, Maybe, True));

   Or_Table : constant array (Boolean_Maybe, Boolean_Maybe) of Boolean_Maybe :=
              ((False, Maybe, True),
               (Maybe, Maybe, True),
               (True,  True,  True));


   function "not" (Right : Boolean_Maybe) return Boolean_Maybe is
   begin
      return Not_Table(Right);
   end "not";


   function "and" (Left, Right : Boolean_Maybe) return Boolean_Maybe is
   begin
      return And_Table(Left, Right);
   end "and";


   function "or" (Left, Right : Boolean_Maybe) return Boolean_Maybe is
   begin
      return Or_Table(Left, Right);
   end "or";


   function To_Boolean_Maybe (Condition : Boolean) return Boolean_Maybe is
   begin
      if Condition then
         return True;
      else
         return False;
      end if;
   end To_Boolean_Maybe;


   function To_Boolean (Condition  : Boolean_Maybe;
                        Optimistic : Boolean) return Boolean is
      -- Optimistic : Maybe -> True, else Maybe -> False.
   begin
      case Condition is
         when False =>
            return False;
         when Maybe =>
            return Optimistic;
         when True =>
            return True;
      end case;
   end To_Boolean;

end Blurred_Logic;
