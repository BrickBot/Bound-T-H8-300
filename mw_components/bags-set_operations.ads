-- Filename        : bags-set_operations.ads
-- Description     :
-- Author          : Mats Weber
-- Created On      : Wed Jul  1 16:50:21 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jul  1 16:52:12 1998
-- Update Count    : 2


generic
package Bags.Set_Operations is
---------------------------

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

end Bags.Set_Operations;
