-- GENERIC PACKAGE FOR QUEUES
   --------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jun  8 11:46:32 1998
-- Update Count    : 10

-- Revision : 19-May-1998 by Mats Weber, made full view of type Queue limited.
-- Revision : 18-Nov-1993 by Mats Weber, added generic procedure Update_All.
-- Revision :  8-SEP-1988 by Mats Weber, added procedure GET(QUEUE).
-- Revision : 31-AUG-1988 by Mats Weber, added procedures ASSIGN and SWAP.
-- Revision : 12-APR-1988 by Mats Weber, added function "=" (QUEUE, QUEUE) : BOOLEAN
--                                       and generic formal type COUNT,
--                                       and changed parameter names.

-- Creation : 28-JUL-1985 by Mats Weber


generic
   type Item_Type is private;
   type Count is range <>;     -- must include 0
package Queues is
--------------

   type Queue is limited private;
   ----------

   subtype Natural_Count is Count range 0 .. Count'Last;


   procedure Put (Item : in Item_Type; Into : in out Queue);
   procedure Get (From : in out Queue; Item : out Item_Type);
   procedure Get (From : in out Queue);

   function First  (Of_Queue  : Queue) return Item_Type;
   function Last   (Of_Queue  : Queue) return Item_Type;
   function Length (Of_Queue  : Queue) return Natural_Count;
   function Empty  (The_Queue : Queue) return Boolean;

   function "=" (Left, Right : Queue) return Boolean;
      -- Returns TRUE if and only if both queues have the same
      -- contents in the same sequence.

   generic
      with procedure Action (Item : in Item_Type);
   procedure Traversal (On_Queue : in Queue);
      -- Calls ACTION for all items in the queue, from FIRST to LAST.

   generic
      with procedure Update_Item (Item : in out Item_Type);
   procedure Update_All (On_Queue : in out Queue);
      -- Calls UPDATE_ITEM for all items in the queue, from FIRST to LAST.

   procedure Assign (Object : in out Queue; Value : in Queue);
      -- OBJECT := VALUE.

   procedure Swap (Left, Right : in out Queue);
      -- Exchanges LEFT and RIGHT.

   procedure Destroy (The_Queue : in out Queue);
      -- Removes all items from THE_QUEUE.

   Queue_Empty : exception;

private

   type Cell;

   type Link is access Cell;

   type Cell is
      record
         Val  : Item_Type;
         Next : Link;
      end record;

   type Queue is limited
      record
         First, Last : Link;
         Length      : Natural_Count := 0;
      end record;


   pragma Inline(Put, Get, First, Last, Length, Empty, Swap);

end Queues;
