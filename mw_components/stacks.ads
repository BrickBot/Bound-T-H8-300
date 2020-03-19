-- GENERIC PACKAGE FOR STACKS
--------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jun  8 11:46:32 1998
-- Update Count    : 6

-- Revision : 28-May-1998 by Mats Weber, made full view of type Stack limited.
-- Revision : 18-Nov-1993 by Mats Weber, added generic procedure Update_All.
-- Revision :  6-SEP-1988 by Mats Weber, added procedure POP(STACK).
-- Revision : 31-AUG-1988 by Mats Weber, added procedures ASSIGN and SWAP.
-- Revision : 12-APR-1988 by Mats Weber, added function "=" (STACK, STACK) : BOOLEAN
--                                       and generic formal type COUNT,
--                                       and changed parameter names.

-- Creation : 28-JUL-1985 by Mats Weber


generic
   type Item_Type is private;
   type Count is range <>;     -- must include 0
package Stacks is
--------------

   type Stack is limited private;
   ----------

   subtype Natural_Count is Count range 0 .. Count'Last;


   procedure Push (Item : in Item_Type; On : in out Stack);
   procedure Pop (From : in out Stack; Item : out Item_Type);
   procedure Pop (From : in out Stack);

   function Top   (Of_Stack  : Stack) return Item_Type;
   function Card  (Of_Stack  : Stack) return Natural_Count;
   function Empty (The_Stack : Stack) return Boolean;

   function "=" (Left, Right : Stack) return Boolean;
      -- Returns TRUE if and only if both stacks have the same
      -- contents in the same sequence.

   generic
      with procedure Action (Item : in Item_Type);
   procedure Traversal (On_Stack : in Stack);
      -- Calls ACTION for all items in the stack, beginning at the top.

   generic
      with procedure Update_Item (Item : in out Item_Type);
   procedure Update_All (On_Stack : in out Stack);
      -- Calls UPDATE_ITEM for all items in the stack, beginning at the top.

   procedure Assign (Object : in out Stack; Value : in Stack);
      -- OBJECT := VALUE

   procedure Swap (Left, Right : in out Stack);
      -- Exchanges LEFT and RIGHT.

   procedure Destroy (The_Stack : in out Stack);
      -- Removes all items from THE_STACK.

   Stack_Underflow : exception;

private

   type Cell;

   type Link is access Cell;

   type Cell is
      record
         Val  : Item_Type;
         Next : Link;
      end record;

   type Stack is limited
      record
         Top      : Link;
         Cardinal : Natural_Count := 0;
      end record;


   pragma Inline(Push, Pop, Top, Card, Empty, Swap);

end Stacks;
