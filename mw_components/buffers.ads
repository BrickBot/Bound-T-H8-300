-- GENERIC FIFO BUFFER TASK TYPE
   -----------------------------

-- Revision : 11-May-1992 by Mats Weber, added function Current_Size.

-- Creation : 21-MAR-1989 by Mats Weber.


generic
   type Item_Type is private;
   type Count is range <>;    -- must contain 0
package Buffers is
---------------

   subtype Natural_Count is Count range 0..Count'Last;

   Unbounded : constant Natural_Count := 0;

   task type Buffer is
   ----------------

      entry Set_Size (To : in Natural_Count);
         -- Sets the buffer's size. Must be called before using
         -- the buffer.
         -- If TO = UNBOUNDED, then the number of elements that
         -- the buffer can contain is unlimited.

      entry Write (Item : in Item_Type);
         -- Writes ITEM to the buffer.
         -- If the buffer is full, the calling task will wait
         -- until an item is read, unless the buffer's size
         -- is unbounded.

      entry Read (Item : out Item_Type);
         -- Returns the next item from the buffer's queue.
         -- If the buffer is empty, the calling task will wait
         -- until an item is written.

      entry Get_Current_Size (Size : out Natural_Count);
         -- Returns the current number of items in the buffer.
         -- Can only be called after Set_Size.

      entry Kill;
         -- Terminates the buffer task. This call will be accepted
         -- only if the buffer is empty.
         -- This entry is useful only for buffers whose masters are
         -- library packages, because the body of BUFFER contains
         -- a terminate alternative.

   end Buffer;


   function Current_Size (Of_Buffer : in Buffer) return Natural_Count;
      -- Calls Of_Buffer.Get_Current_Size and returns
      -- the result.

end Buffers;
