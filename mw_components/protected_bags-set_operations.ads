-- Filename        : protected_bags-set_operations.ads
-- Description     :
-- Author          : Mats Weber
-- Created On      : Fri Jul  3 15:38:50 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Fri Jul  3 15:48:41 1998
-- Update Count    : 2


generic
package Protected_Bags.Set_Operations is
-------------------------------------

   -- Set comparison functions
   -- (two set elements are equal if their keys are equal)
   -- Duplicate keys are treated as if they were alone.

   function "=" (Left, Right : Bag) return Boolean;
      -- set equality.

   function "<" (Left, Right : Bag) return Boolean;
   function ">" (Left, Right : Bag) return Boolean;
      -- strict inclusion.

   function "<=" (Left, Right : Bag) return Boolean;
   function ">=" (Left, Right : Bag) return Boolean;
      -- normal set inclusion.

end Protected_Bags.Set_Operations;
