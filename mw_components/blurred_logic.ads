-- Blurred Logic (False, Maybe, True).
   -----------------------------------

-- blurred_logic_.a
-- Author          : Mats Weber
-- Created On      : Tue Jun 11 14:27:04 1996
-- Last Modified By: Mats Weber
-- Last Modified On: Tue Jun 11 14:35:54 1996
-- Update Count    : 2


package Blurred_Logic is
---------------------

   type Boolean_Maybe is (False, Maybe, True);


   function "not" (Right : Boolean_Maybe) return Boolean_Maybe;

   function "and" (Left, Right : Boolean_Maybe) return Boolean_Maybe;

   function "or" (Left, Right : Boolean_Maybe) return Boolean_Maybe;


   function To_Boolean_Maybe (Condition : Boolean) return Boolean_Maybe;

   function To_Boolean (Condition  : Boolean_Maybe;
                        Optimistic : Boolean) return Boolean;
      -- Optimistic : Maybe -> True, else Maybe -> False.

   pragma Inline("not", "and", "or",
                 To_Boolean_Maybe, To_Boolean);

end Blurred_Logic;
