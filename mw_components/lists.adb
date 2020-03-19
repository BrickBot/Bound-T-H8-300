-- GENERIC PACKAGE FOR LISTS WITH POINTERS
   ---------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jun  8 11:42:38 1998
-- Update Count    : 10

-- Creation : 14-MAR-1988 by Mats Weber.


with Exchange,
     Unchecked_Deallocation;

package body Lists is
------------------

   procedure Nullify (The_List : out List) is
      -- Not exported.
   begin
      The_List.Head := null;
      The_List.Tail := null;
      The_List.Length := 0;
   end Nullify;

   pragma Inline(Nullify);


   procedure Insert_Before (Item   : in Item_Type;
                            Into   : in out List;
                            Before : in Pointer) is
   begin
      if Link(Before) = Into.Head then
         Insert_At_Head(Item, Into);
      elsif Before = null then
         raise Pointer_Is_Null;
      else
         Into.Length := Into.Length + 1;   -- may raise CONSTRAINT_ERROR
         declare

             New_Cell : constant Link := new Cell'(Value    => Item,
                                                   Next     => Link(Before),
                                                   Previous => Before.Previous);

         begin
            Before.Previous.Next := New_Cell;
            Before.Previous      := New_Cell;
         end;
      end if;
   end Insert_Before;


   procedure Insert_After (Item  : in Item_Type;
                           Into  : in out List;
                           After : in Pointer) is
   begin
      if Link(After) = Into.Tail then
         Insert_At_Tail(Item, Into);
      elsif After = null then
         raise Pointer_Is_Null;
      else
         Into.Length := Into.Length + 1;   -- may raise CONSTRAINT_ERROR
         declare

             New_Cell : constant Link := new Cell'(Value    => Item,
                                                   Next     => After.Next,
                                                   Previous => Link(After));

         begin
            After.Next.Previous := New_Cell;
            After.Next          := New_Cell;
         end;
      end if;
   end Insert_After;


   procedure Insert_At_Head (Item : in Item_Type;
                             Into : in out List) is
   begin
      Into.Length := Into.Length + 1;   -- may raise CONSTRAINT_ERROR
      if Into.Length = 1 then
         Into.Head := new Cell;
         Into.Tail := Into.Head;
         Into.Head.Value    := Item;
         Into.Head.Next     := Into.Head;
         Into.Head.Previous := Into.Head;
      else
         declare

            New_Cell : constant Link := new Cell'(Value    => Item,
                                                  Next     => Into.Head,
                                                  Previous => Into.Tail);

         begin
            Into.Tail.Next     := New_Cell;
            Into.Head.Previous := New_Cell;
            Into.Head          := New_Cell;
         end;
      end if;
   end Insert_At_Head;


   procedure Insert_At_Tail (Item : in Item_Type;
                             Into : in out List) is
   begin
      Into.Length := Into.Length + 1;   -- may raise CONSTRAINT_ERROR
      if Into.Length = 1 then
         Into.Head := new Cell;
         Into.Tail := Into.Head;
         Into.Head.Value    := Item;
         Into.Head.Next     := Into.Head;
         Into.Head.Previous := Into.Head;
      else
         declare

            New_Cell : constant Link := new Cell'(Value    => Item,
                                                  Next     => Into.Head,
                                                  Previous => Into.Tail);

         begin
            Into.Tail.Next     := New_Cell;
            Into.Head.Previous := New_Cell;
            Into.Tail          := New_Cell;
         end;
      end if;
   end Insert_At_Tail;


   procedure Dispose is new Unchecked_Deallocation(Cell, Link);


   procedure Remove (Position  : in out Pointer;
                     From      : in out List;
                     Direction : in Scan_Direction := Forward) is
   begin
      if Position = null then
         raise Pointer_Is_Null;
      end if;
      if Link(Position) = From.Head then
         if From.Length = 1 then
            Dispose(Link(Position));
            Nullify(From);
            return;
         else
            From.Head := From.Head.Next;
         end if;
      elsif Link(Position) = From.Tail then
         From.Tail := From.Tail.Previous;
      end if;
      Position.Previous.Next := Position.Next;
      Position.Next.Previous := Position.Previous;
      From.Length := From.Length - 1;
      declare

         Trash : Pointer := Position;

      begin
         case Direction is
            when Forward =>
               Position := Pointer(Position.Next);
            when Backward =>
               Position := Pointer(Position.Previous);
         end case;
         Dispose(Link(Trash));
      end;
   end Remove;


   procedure Remove (Position     : in out Pointer;
                     From         : in out List;
                     Removed_Item : out Item_Type;
                     Direction    : in Scan_Direction := Forward) is
   begin
      if Position = null then
         raise Pointer_Is_Null;
      end if;
      Removed_Item := Position.Value;
      Remove(Position, From, Direction);
   end Remove;


   procedure Remove_Head (From : in out List) is
   begin
      if From.Length = 0 then
         raise List_Empty;
      else
         declare

            Trash : Link := From.Head;

         begin
            if From.Length = 1 then
               Nullify(From);
            else
               From.Head          := From.Head.Next;
               From.Tail.Next     := From.Head;
               From.Head.Previous := From.Tail;
               From.Length := From.Length - 1;
            end if;
            Dispose(Trash);
         end;
      end if;
   end Remove_Head;


   procedure Remove_Head (From         : in out List;
                          Removed_Item : out Item_Type) is
   begin
      if From.Length = 0 then
         raise List_Empty;
      end if;
      Removed_Item := From.Head.Value;
      Remove_Head(From);
   end Remove_Head;


   procedure Remove_Tail (From : in out List) is
   begin
      if From.Length = 0 then
         raise List_Empty;
      else
         declare

            Trash : Link := From.Tail;

         begin
            if From.Length = 1 then
               Nullify(From);
            else
               From.Tail          := From.Tail.Previous;
               From.Head.Previous := From.Tail;
               From.Tail.Next     := From.Head;
               From.Length        := From.Length - 1;
            end if;
            Dispose(Trash);
         end;
      end if;
   end Remove_Tail;


   procedure Remove_Tail (From         : in out List;
                          Removed_Item : out Item_Type) is
   begin
      if From.Length = 0 then
         raise List_Empty;
      end if;
      Removed_Item := From.Tail.Value;
      Remove_Tail(From);
   end Remove_Tail;


   function Element (Position : Pointer) return Item_Type is
   begin
      if Position = null then
         raise Pointer_Is_Null;
      end if;
      return Position.Value;
   end Element;


   procedure Assign (Position : in Pointer; Value : in Item_Type) is
   begin
      if Position = null then
         raise Pointer_Is_Null;
      end if;
      Position.Value := Value;
   end Assign;


   function Head (Of_List : List) return Pointer is
   begin
      return Pointer(Of_List.Head);
   end Head;


   function Tail (Of_List : List) return Pointer is
   begin
      return Pointer(Of_List.Tail);
   end Tail;


   function Next (Position : Pointer) return Pointer is
   begin
      if Position = null then
         raise Pointer_Is_Null;
      end if;
      return Pointer(Position.Next);
   end Next;


   function Previous (Position : Pointer) return Pointer is
   begin
      if Position = null then
         raise Pointer_Is_Null;
      end if;
      return Pointer(Position.Previous);
   end Previous;


   function Length (Of_List : List) return Natural_Count is
   begin
      return Of_List.Length;
   end Length;


   function Empty (The_List : List) return Boolean is
   begin
      return The_List.Length = 0;
   end Empty;


   function "=" (Left, Right : List) return Boolean is
   begin
      if Left.Length = Right.Length then
         declare

            Z_Left  : Link := Left.Head;
            Z_Right : Link := Right.Head;

         begin
            for I in 1 .. Left.Length loop
               if Z_Left.Value /= Z_Right.Value then
                  return False;
               end if;
               Z_Left  := Z_Left.Next;
               Z_Right := Z_Right.Next;
            end loop;
            return True;
         end;
      else
         return False;
      end if;
   end "=";


   generic
      with procedure Link_Action (L : in Link);
   procedure Link_Traversal (In_List   : in List;
                             Direction : in Scan_Direction);

   procedure Link_Traversal (In_List   : in List;
                             Direction : in Scan_Direction) is
   begin
      case Direction is
         when Forward =>
            declare

               Z : Link := In_List.Head;

            begin
               for I in 1 .. In_List.Length loop
                  Link_Action(Z);
                  Z := Z.Next;
               end loop;
            end;
         when Backward =>
            declare

               Z : Link := In_List.Tail;

            begin
               for I in 1 .. In_List.Length loop
                  Link_Action(Z);
                  Z := Z.Previous;
               end loop;
            end;
      end case;
   end Link_Traversal;


   procedure Traversal (In_List   : in List;
                        Direction : in Scan_Direction := Forward) is

      procedure Action_On_Link (L : in Link) is
      begin
         Action(L.Value);
      end;

      pragma Inline(Action_On_Link);

      procedure Traverse is new Link_Traversal(Action_On_Link);

   begin
      Traverse(In_List, Direction);
   end Traversal;


   procedure Update_All (In_List   : in out List;
                         Direction : in Scan_Direction := Forward) is

      procedure Modify_On_Link (L : in Link) is
      begin
         Modify(L.Value);
      end;

      pragma Inline(Modify_On_Link);

      procedure Update is new Link_Traversal(Modify_On_Link);

   begin
      Update(In_List, Direction);
   end Update_All;


   function Search (Item      : Item_Type;
                    Within    : List;
                    Direction : Scan_Direction := Forward) return Pointer is

      function Equal_To_Item (X : Item_Type) return Boolean is
      begin
         return X = Item;
      end;

      pragma Inline(Equal_To_Item);

      function Search_For_Item is new Scan(Equal_To_Item);

   begin
      return Search_For_Item(Within, Direction);
   end Search;


   function Scan (Within    : List;
                  Direction : Scan_Direction := Forward) return Pointer is

      Item_Found : exception;

      Result_Link : Link;


      procedure Check_Condition (L : in Link) is
      begin
         if Condition(L.Value) then
            Result_Link := L;
            raise Item_Found;
         end if;
      end;

      pragma Inline(Check_Condition);

      procedure Check_All is new Link_Traversal(Check_Condition);

   begin
      Check_All(Within, Direction);
      raise Item_Not_Found;
   exception
      when Item_Found =>
         return Pointer(Result_Link);
   end Scan;


   procedure Assign (Object : in out List; Value : in List) is

      Z : Link := Value.Head;

   begin
      Destroy(Object);
      for I in 1 .. Value.Length loop
         Insert_At_Tail(Item => Z.Value, Into => Object);
         Z := Z.Next;
      end loop;
   end Assign;


   procedure Move_Before (From   : in out List;
                          Into   : in out List;
                          Before : in Pointer) is
   begin
      if From.Length /= 0 then
         if Into.Length = 0 then
            Into.Head   := From.Head;
            Into.Tail   := From.Tail;
            Into.Length := From.Length;
         elsif Before = null then
            raise Pointer_Is_Null;
         else
            Into.Length := Into.Length + From.Length;   -- may raise CONSTRAINT_ERROR
            From.Tail.Next     := Link(Before);
            From.Head.Previous := Before.Previous;
            Before.Previous.Next := From.Head;
            Before.Previous      := From.Tail;
            if Link(Before) = Into.Head then
               Into.Head := From.Head;
            end if;
         end if;
         Nullify(From);
      end if;
   end Move_Before;


   procedure Move_After (From  : in out List;
                         Into  : in out List;
                         After : in Pointer) is
   begin
      if From.Length /= 0 then
         if Into.Length = 0 then
            Into.Head   := From.Head;
            Into.Tail   := From.Tail;
            Into.Length := From.Length;
         elsif After = null then
            raise Pointer_Is_Null;
         else
            Into.Length := Into.Length + From.Length;   -- may raise CONSTRAINT_ERROR
            From.Tail.Next     := After.Next;
            From.Head.Previous := Link(After);
            After.Next.Previous := From.Tail;
            After.Next          := From.Head;
            if Link(After) = Into.Tail then
               Into.Tail := From.Tail;
            end if;
         end if;
         Nullify(From);
      end if;
   end Move_After;


   procedure Swap (Left, Right : in out List) is

      procedure Swap is new Exchange(Link);
      procedure Swap is new Exchange(Natural_Count);

   begin
      Swap(Left.Head, Right.Head);
      Swap(Left.Tail, Right.Tail);
      Swap(Left.Length, Right.Length);
   end Swap;


   procedure Destroy (The_List : in out List) is

      Z     : Link := The_List.Head;
      Trash : Link;

   begin
      for I in 1 .. The_List.Length loop
         Trash := Z;
         Z := Z.Next;
         Dispose(Trash);
      end loop;
      Nullify(The_List);
   end Destroy;

end Lists;
