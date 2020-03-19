-- GENERIC BINARY SEARCH PACKAGE
   -----------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Nov 25 16:06:14 1997
-- Update Count    : 5

-- Revision :  4-JUL-1989 by Mats Weber, added treatment of null arrays.
-- Revision : 12-MAY-1989 by Mats Weber, added type LOCATION and
--                                       function LOCATE return LOCATION.

-- Creation :  8-JUN-1988 by Mats Weber.


generic
   type Item_Type is limited private;
   type Key_Type (<>) is limited private;
   type Index is (<>);
   type Item_Array is array (Index range <>) of Item_Type;
   with function Key_Of (X : Item_Type) return Key_Type;
   with function "=" (X, Y : Key_Type) return Boolean is <>;
   with function "<" (X, Y : Key_Type) return Boolean is <>;
package Binary_Search is
---------------------

   type Location_Kind is (Found, Inbetween, Smaller, Greater, Null_Array);

   type Location (Kind : Location_Kind := Found) is
      record
         case Kind is
            when Found      => Position       : Index;
            when Inbetween  => Before, After  : Index;
            when Smaller |
                 Greater |
                 Null_Array => null;
         end case;
      end record;


   function Locate (Key    : Key_Type;
                    Within : Item_Array) return Location;
      -- If KEY is found in WITHIN then the result's KIND
      -- will be FOUND and POSITION will indicate the position of
      -- the item matching KEY.
      -- If KEY is between two items of WITHIN, then the result's KIND
      -- will be INBETWEEN and BEFORE and AFTER will be such that
      -- AFTER = INDEX'SUCC(BEFORE) and
      -- KEY_OF(WITHIN(BEFORE)) < KEY < KEY_OF(WITHIN(AFTER)).
      -- If KEY is smaller (greater) than all items of WITHIN,
      -- then the result's kind will be SMALLER (GREATER).
      -- If WITHIN is a null array, then the result's kind
      -- will be NULL_ARRAY.

   function Locate (Key    : Key_Type;
                    Within : Item_Array) return Index;
      -- Returns the position of the item matching KEY in WITHIN.
      -- Will raise NOT_FOUND if WITHIN contains no item with
      -- a key equal to KEY.


   Not_Found : exception;

end Binary_Search;
