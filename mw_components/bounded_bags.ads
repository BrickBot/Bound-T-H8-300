-- Generic Bags package based on sorted arrays and binary search
   -------------------------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Thu May 28 18:01:47 1998
-- Update Count    : 2

-- Creation :  3-JUL-1989 by Mats Weber.


generic
   type Key_Type is limited private;
   type Item_Type is private;
   with function Key_Of (Item : in Item_Type) return Key_Type;
   with function "=" (X, Y : Key_Type) return Boolean is <>;
   with function "<" (X, Y : Key_Type) return Boolean is <>;
   type Count is range <>;    -- must include 0
package Bounded_Bags is
--------------------

   subtype Natural_Count is Count range 0..Count'Last;

   type Bag (Size                   : Natural_Count;
             Duplicate_Keys_Allowed : Boolean) is limited private;


   Nonexistent_Key,
   Duplicate_Key    : exception;


   procedure Insert (Item : in Item_Type; Into : in out Bag);

   procedure Remove (Key : in Key_Type; From : in out Bag);

   function Search (Key : Key_Type; Within : Bag) return Item_Type;


   function Card (Of_Bag : Bag) return Natural_Count;

   function Empty (The_Bag : Bag) return Boolean;

private

   function "=" (Left, Right : Item_Type) return Boolean is abstract;
      -- Make sure we don't use equality on Item_Type, we want to use
      -- it only on Key_Type.

   subtype Positive_Count is Count range 1..Count'Last;

   type Item_Array is array (Positive_Count range <>) of Item_Type;

   type Bag (Size                   : Natural_Count;
             Duplicate_Keys_Allowed : Boolean) is
      record
         Last     : Natural_Count := 0;
         Contents : Item_Array(1 .. Size);
      end record;

end Bounded_Bags;
