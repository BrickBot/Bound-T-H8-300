-- GENERIC PACKAGE FOR STACKS
   --------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jun  8 11:47:50 1998
-- Update Count    : 8

-- Revision : 20-SEP-1986 by Mats Weber, corrected PUSH and DESTROY.

-- Creation : 17-FEB-1986 Mats Weber


with Exchange,
     Unchecked_Deallocation;

package body Stacks is
-------------------

   procedure Push (Item : in Item_Type; On : in out Stack) is
   begin
      On.Cardinal := On.Cardinal + 1;   -- may raise CONSTRAINT_ERROR
      On.Top      := new Cell'(Val => Item, Next => On.Top);
   end Push;


   procedure Dispose is new Unchecked_Deallocation(Cell, Link);


   procedure Pop (From : in out Stack; Item : out Item_Type) is

      Z : Link;

   begin
      if From.Top = null then
         raise Stack_Underflow;
      end if;
      Item := From.Top.Val;
      Z := From.Top;
      From.Top      := From.Top.Next;
      From.Cardinal := From.Cardinal - 1;
      Dispose(Z);
   end Pop;

   procedure Pop (From : in out Stack) is

      Z : Link;

   begin
      if From.Top = null then
         raise Stack_Underflow;
      end if;
      Z := From.Top;
      From.Top      := From.Top.Next;
      From.Cardinal := From.Cardinal - 1;
      Dispose(Z);
   end Pop;


   function Top (Of_Stack : Stack) return Item_Type is
   begin
      if Of_Stack.Top = null then
         raise Stack_Underflow;
      end if;
      return Of_Stack.Top.Val;
   end Top;


   function Card (Of_Stack : Stack) return Natural_Count is
   begin
      return Of_Stack.Cardinal;
   end Card;


   function Empty (The_Stack : Stack) return Boolean is
   begin
      return The_Stack.Top = null;
   end Empty;


   function "=" (Left, Right : Stack) return Boolean is
   begin
      if Left.Cardinal /= Right.Cardinal then
         return False;
      end if;
      declare

         Z_Left  : Link := Left.Top;
         Z_Right : Link := Right.Top;

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


   procedure Traversal (On_Stack : in Stack) is

      Z : Link := On_Stack.Top;

   begin
      while Z /= null loop
         Action(Z.Val);
         Z := Z.Next;
      end loop;
   end Traversal;


   procedure Update_All (On_Stack : in out Stack) is

      Z : Link := On_Stack.Top;

   begin
      while Z /= null loop
         Update_Item(Z.Val);
         Z := Z.Next;
      end loop;
   end Update_All;


   procedure Assign (Object : in out Stack; Value : in Stack) is
   begin
      Destroy(Object);
      if Value.Top /= null then
         declare

            Z : Link := Value.Top.Next;
            P : Link := new Cell'(Val => Value.Top.Val, Next => null);

         begin
            Object.Top := P;
            while Z /= null loop
               P.Next := new Cell'(Val => Z.Val, Next => null);
               P      := P.Next;
               Z      := Z.Next;
            end loop;
            Object.Cardinal := Value.Cardinal;
         end;
      end if;
   end Assign;


   procedure Swap (Left, Right : in out Stack) is

      procedure Swap is new Exchange(Link);
      procedure Swap is new Exchange(Natural_Count);

   begin
      Swap(Left.Top, Right.Top);
      Swap(Left.Cardinal, Right.Cardinal);
   end Swap;


   procedure Destroy (The_Stack : in out Stack) is

      Z : Link;

   begin
      while The_Stack.Top /= null loop
         Z := The_Stack.Top;
         The_Stack.Top := The_Stack.Top.Next;
         Dispose(Z);
      end loop;
      The_Stack.Cardinal := 0;
   end Destroy;

end Stacks;
