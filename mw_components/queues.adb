-- GENERIC PACKAGE FOR QUEUES
   --------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jun  8 11:47:50 1998
-- Update Count    : 10

-- Revision : 20-SEP-1986 by Mats Weber, corrected DESTROY.

-- Creation : 10-FEB-1986 by Mats Weber.


with Exchange,
     Unchecked_Deallocation;

package body Queues is
-------------------

   procedure Put (Item : in Item_Type; Into : in out Queue) is
   begin
      Into.Length := Into.Length + 1;   -- may raise CONSTRAINT_ERROR
      if Into.Last /= null then
         Into.Last.Next := new Cell'(Val => Item, Next => null);
         Into.Last      := Into.Last.Next;
      else
         Into.Last  := new Cell'(Val => Item, Next => null);
         Into.First := Into.Last;
      end if;
   end Put;


   procedure Dispose is new Unchecked_Deallocation(Cell, Link);


   procedure Get (From : in out Queue; Item : out Item_Type) is

      Z : Link := From.First;

   begin
      if From.First = null then
         raise Queue_Empty;
      end if;
      Item := From.First.Val;
      From.First := From.First.Next;
      if From.First = null then
         From.Last := null;
      end if;
      From.Length := From.Length - 1;
      Dispose(Z);
   end Get;

   procedure Get (From : in out Queue) is

      Z : Link := From.First;

   begin
      if From.First = null then
         raise Queue_Empty;
      end if;
      From.First := From.First.Next;
      if From.First = null then
         From.Last := null;
      end if;
      From.Length := From.Length - 1;
      Dispose(Z);
   end Get;


   function First (Of_Queue : Queue) return Item_Type is
   begin
      if Of_Queue.First = null then
         raise Queue_Empty;
      end if;
      return Of_Queue.First.Val;
   end;


   function Last (Of_Queue : Queue) return Item_Type is
   begin
      if Of_Queue.Last = null then
         raise Queue_Empty;
      end if;
      return Of_Queue.Last.Val;
   end;


   function Length (Of_Queue : Queue) return Natural_Count is
   begin
      return Of_Queue.Length;
   end;


   function Empty (The_Queue : Queue) return Boolean is
   begin
      return The_Queue.First = null;
   end;


   function "=" (Left, Right : Queue) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;
      declare

         Z_Left  : Link := Left.First;
         Z_Right : Link := Right.First;

      begin
         while Z_Left /= null loop
            if Z_Left.Val /= Z_Right.Val then
               return False;
            end if;
            Z_Left  := Z_Left.Next;
            Z_Right := Z_Right.Next;
         end loop;
         return True;
      end;
   end "=";


   procedure Traversal (On_Queue : in Queue) is

      P : Link := On_Queue.First;

   begin
      while P /= null loop
         Action(P.Val);
         P := P.Next;
      end loop;
   end Traversal;


   procedure Update_All (On_Queue : in out Queue) is

      P : Link := On_Queue.First;

   begin
      while P /= null loop
         Update_Item(P.Val);
         P := P.Next;
      end loop;
   end Update_All;


   procedure Assign (Object : in out Queue; Value : in Queue) is
   begin
      Destroy(Object);
      if Value.First /= null then
         declare

            Z : Link := Value.First.Next;
            P : Link := new Cell'(Val => Value.First.Val, Next => null);

         begin
            Object.First := P;
            while Z /= null loop
               P.Next := new Cell'(Val => Z.Val, Next => null);
               P      := P.Next;
               Z      := Z.Next;
            end loop;
            Object.Last   := P;
            Object.Length := Value.Length;
         end;
      end if;
   end Assign;


   procedure Swap (Left, Right : in out Queue) is

      procedure Swap is new Exchange(Link);
      procedure Swap is new Exchange(Natural_Count);

   begin
      Swap(Left.First, Right.First);
      Swap(Left.Last, Right.Last);
      Swap(Left.Length, Right.Length);
   end Swap;


   procedure Destroy (The_Queue : in out Queue) is

      Z : Link;

   begin
      while The_Queue.First /= null loop
         Z := The_Queue.First;
         The_Queue.First := The_Queue.First.Next;
         Dispose(Z);
      end loop;
      The_Queue.Last   := null;
      The_Queue.Length := 0;
   end Destroy;

end Queues;
