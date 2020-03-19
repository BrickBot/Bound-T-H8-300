-- GENERIC BAGS PACKAGE IMPLEMENTED WITH AVL TREES
   -----------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jul  1 17:33:03 1998
-- Update Count    : 21

-- Revision : 15-Apr-1992 by Mats Weber, Corrected bug in Assign(Bag, List) (forgot
--                                       to destroy temporary object on exception).
-- Revision : 21-FEB-1988 by Mats Weber, optimized traversal procedures.

-- Creation :  2-JUL-1985 by Mats Weber


with Exchange;

package body Bags is
-----------------

   use Implementation;


   procedure Insert (Item : in Item_Type; Into : in out Bag) is

      Depth_Increased : Boolean := False;

      -- copied from N.Wirth, "Algorithms + Data Structures = Programs", p 220

      procedure Insert (P : in out Link) is
      begin
         if P = null then
            Into.Card := Into.Card + 1;            -- may raise CONSTRAINT_ERROR
            P := new Cell'(Val          => Item,
                           Balance      => 0,
                           Left | Right => null);
            Depth_Increased := True;
         elsif Key_Of(Item) < Key_Of(P.Val) then
            -- insert into left subtree
            Insert(P.Left);
            if Depth_Increased then
               case P.Balance is
                  when +1 =>
                     P.Balance := 0;
                     Depth_Increased := False;
                  when 0 =>
                     P.Balance := -1;
                  when -1 =>
                     -- rebalance
                     declare

                        P1, P2 : Link;

                     begin
                        P1 := P.Left;
                        if P1.Balance = -1 then
                           -- single LL rotation
                           P.Left    := P1.Right;
                           P1.Right  := P;
                           P.Balance := 0;
                           P := P1;
                        else
                           -- double LR rotation
                           P2 := P1.Right;
                           P1.Right := P2.Left;
                           P2.Left  := P1;
                           P.Left   := P2.Right;
                           P2.Right := P;
                           if P2.Balance = -1 then
                              P.Balance := 1;
                           else
                              P.Balance := 0;
                           end if;
                           if P2.Balance = 1 then
                              P1.Balance := -1;
                           else
                              P1.Balance := 0;
                           end if;
                           P := P2;
                        end if;
                     end;
                     P.Balance := 0;
                     Depth_Increased := False;
               end case;
            end if;
         elsif not Into.Duplicate_Keys_Allowed and then Key_Of(Item) = Key_Of(P.Val) then
            raise Duplicate_Key;
         else
            -- insert into right subtree
            Insert(P.Right);
            if Depth_Increased then
               case P.Balance is
                  when -1 =>
                     P.Balance := 0;
                     Depth_Increased := False;
                  when 0 =>
                     P.Balance := 1;
                  when +1 =>
                     -- rebalance
                     declare

                        P1, P2 : Link;

                     begin
                        P1 := P.Right;
                        if P1.Balance = 1 then
                           -- single RR rotation
                           P.Right   := P1.Left;
                           P1.Left   := P;
                           P.Balance := 0;
                           P := P1;
                        else
                           -- double RL rotation
                           P2 := P1.Left;
                           P1.Left  := P2.Right;
                           P2.Right := P1;
                           P.Right  := P2.Left;
                           P2.Left  := P;
                           if P2.Balance = 1 then
                              P.Balance := -1;
                           else
                              P.Balance := 0;
                           end if;
                           if P2.Balance = -1 then
                              P1.Balance := +1;
                           else
                              P1.Balance := 0;
                           end if;
                           P := P2;
                        end if;
                     end;
                     P.Balance := 0;
                     Depth_Increased := False;
               end case;
            end if;
         end if;
      end Insert;

   begin
      Insert(Into.Root);
   end Insert;


   procedure Insert (Items : in List; Into : in out Bag) is
   begin
      if not Into.Duplicate_Keys_Allowed then
         declare

            Temp : Bag(Duplicate_Keys_Allowed => False);

         begin
            for I in Items'Range loop
               Insert(Items(I), Into => Temp);
            end loop;
            Destroy(Temp);
         exception
            when Duplicate_Key =>
               Destroy(Temp);
               raise;
         end;
         -- At this point ITEMS cannot have any duplicate keys
         for I in Items'Range loop
            if Member(Key_Of(Items(I)), Of_Bag => Into) then
               raise Duplicate_Key;
            end if;
         end loop;
      end if;
      for I in Items'Range loop
         Insert(Items(I), Into);
      end loop;
   end Insert;


   procedure Insert (Items : in Bag; Into : in out Bag) is

      procedure Add_Subtree (L : in Link) is

         Z : Link := L;

      begin
         while Z /= null loop
            Add_Subtree(Z.Left);
            Insert(Z.Val, Into);
            Z := Z.Right;
         end loop;
      end;

   begin
      if Into.Duplicate_Keys_Allowed then
         Add_Subtree(Items.Root);
      elsif Items.Duplicate_Keys_Allowed then
         Insert(To_List(Items), Into);
      else
         declare

            procedure Check_Subtree (L : in Link) is

               Z : Link := L;

            begin
               while Z /= null loop
                  if Member(Key_Of(Z.Val), Of_Bag => Into) then
                     raise Duplicate_Key;
                  end if;
                  Check_Subtree(Z.Left);
                  Z := Z.Right;
               end loop;
            end;

         begin
            Check_Subtree(Items.Root);
            Add_Subtree(Items.Root);
         end;
      end if;
   end Insert;


   procedure Remove (Key : in Key_Type; From : in out Bag) is

      Junk : Item_Type;

   begin
      Remove(Key, From, Removed_Item => Junk);
   end Remove;


   procedure Remove (Key : in Key_Type; From : in out Bag; Removed_Item : out Item_Type) is

      Depth_Decreased : Boolean := False;

      Key_Found,
      Key_Deleted : Boolean := False;

      -- copied from N.Wirth, "Algorithms + Data Structures = Programs", p 223

      procedure Remove (P : in out Link) is
      begin
         if P = null then
            if not Key_Found then
               raise Nonexistent_Key;
            end if;
         elsif Key < Key_Of(P.Val) then
            -- delete in left subtree
            Remove(P.Left);
            if Depth_Decreased then
               Balance_1(P, Depth_Decreased);
            end if;
         elsif Key = Key_Of(P.Val) then
            Key_Found := True;
            if From.Duplicate_Keys_Allowed then
               -- try deleting in left subtree
               Remove(P.Left);
            end if;
            if Key_Deleted then
               if Depth_Decreased then
                  Balance_1(P, Depth_Decreased);
               end if;
            else
               -- delete P.all
               Removed_Item := P.Val;
               declare

                  Q : Link;

               begin
                  if P.Right = null then
                     Q := P;
                     P := P.Left;
                     Depth_Decreased := True;
                  elsif P.Left = null then
                     Q := P;
                     P := P.Right;
                     Depth_Decreased := True;
                  else
                     declare

                        procedure Delete (R : in out Link) is
                        begin
                           if R.Right = null then
                              P.Val := R.Val;
                              Q := R; -- in order to dispose the right cell
                              R := R.Left;
                              Depth_Decreased := True;
                           else
                              Delete(R.Right);
                              if Depth_Decreased then
                                 Balance_2(R, Depth_Decreased);
                              end if;
                           end if;
                        end Delete;

                     begin
                        Delete(P.Left);
                        if Depth_Decreased then
                           Balance_1(P, Depth_Decreased);
                        end if;
                     end;
                  end if;
                  From.Card := From.Card - 1;
                  Dispose(Q);
               end;
               Key_Deleted := True;
            end if;
         else
            -- delete in right subtree
            Remove(P.Right);
            if Depth_Decreased then
               Balance_2(P, Depth_Decreased);
            end if;
         end if;
      end Remove;

   begin
      Remove(From.Root);
   end Remove;


   function Search (Key : Key_Type; Within : Bag) return Item_Type is

      Z : constant Link := Pointer(Key,
                                   Within.Root,
                                   First_Match => not Within.Duplicate_Keys_Allowed);

   begin
      if Z /= null then
         return Z.Val;
      else
         raise Nonexistent_Key;
      end if;
   end Search;


   function Search (Key : Key_Type; Within : Bag) return List is

      function Equiv (L : Link) return List is
      begin
         if L = null then
            declare

               Null_List : List(1 .. 0);

            begin
               return Null_List;
            end;
         else
            if Key < Key_Of(L.Val) then
               return Equiv(L.Left);
            elsif Key = Key_Of(L.Val) then
               return Equiv(L.Left) & L.Val & Equiv(L.Right);
            else
               return Equiv(L.Right);
            end if;
         end if;
      end Equiv;

   begin
      return Equiv(Pointer(Key, Within.Root, First_Match => True));
   end Search;


   procedure Update (Key : in Key_Type; Within : in out Bag) is

      Z        : constant Link := Pointer(Key,
                                          Within.Root,
                                          First_Match => not Within.Duplicate_Keys_Allowed);
      Old_Item : Item_Type;

   begin
      if Z = null then
         raise Nonexistent_Key;
      else
         Old_Item := Z.Val;
         Modify(Z.Val);
         if Key_Of(Old_Item) /= Key_Of(Z.Val) then
            Z.Val := Old_Item;
            raise Invalid_Key;
         end if;
      end if;
   end Update;


   procedure Replace (Key      : in Key_Type;
                      New_Item : in Item_Type;
                      Within   : in out Bag) is

      Z : constant Link := Pointer(Key,
                                   Within.Root,
                                   First_Match => not Within.Duplicate_Keys_Allowed);

   begin
      if Z = null then
         raise Nonexistent_Key;
      elsif Key_Of(New_Item) /= Key_Of(Z.Val) then
         raise Invalid_Key;
      else
         Z.Val := New_Item;
      end if;
   end Replace;


   function Empty (The_Bag : Bag) return Boolean is
   begin
      return The_Bag.Root = null;
   end Empty;


   function Card (Of_Bag : Bag) return Natural_Count is
   begin
      return Of_Bag.Card;
   end Card;


   function Member (Key : Key_Type; Of_Bag : Bag) return Boolean is
   begin
      return Pointer(Key, Of_Bag.Root, First_Match => True) /= null;
   end Member;


   function Min (Of_Bag : Bag) return Item_Type is

      Z : Link := Of_Bag.Root;

   begin
      if Z = null then
         raise Bag_Empty;
      end if;
      while Z.Left /= null loop
         Z := Z.Left;
      end loop;
      return Z.Val;
   end Min;


   function Max (Of_Bag : Bag) return Item_Type is

      Z : Link := Of_Bag.Root;

   begin
      if Z = null then
         raise Bag_Empty;
      end if;
      while Z.Right /= null loop
         Z := Z.Right;
      end loop;
      return Z.Val;
   end Max;


   procedure Remove_Min (From : in out Bag) is

      Junk : Item_Type;

   begin
      Remove_Min(From, Min => Junk);
   end Remove_Min;


   procedure Remove_Min (From : in out Bag; Min : out Item_Type) is

      Depth_Decreased : Boolean := False;

      procedure Remove_Min (L : in out Link) is
      begin
         if L.Left = null then
            -- delete L.all
            declare

               Q : Link := L;

            begin
               Min := L.Val;
               L   := L.Right;
               Depth_Decreased := True;
               From.Card := From.Card - 1;
               Dispose(Q);
            end;
         else
            -- delete in left subtree
            Remove_Min(L.Left);
            if Depth_Decreased then
               Balance_1(L, Depth_Decreased);
            end if;
         end if;
      end Remove_Min;

   begin
      if From.Root /= null then
         Remove_Min(From.Root);
      else
         raise Bag_Empty;
      end if;
   end Remove_Min;


   procedure Remove_Max (From : in out Bag) is

      Junk : Item_Type;

   begin
      Remove_Max(From, Max => Junk);
   end Remove_Max;


   procedure Remove_Max (From : in out Bag; Max : out Item_Type) is

      Depth_Decreased : Boolean := False;

      procedure Remove_Max (L : in out Link) is
      begin
         if L.Right = null then
            -- delete L.all
            declare

               Q : Link := L;

            begin
               Max := L.Val;
               L   := L.Left;
               Depth_Decreased := True;
               From.Card := From.Card - 1;
               Dispose(Q);
            end;
         else
            -- delete in right subtree
            Remove_Max(L.Right);
            if Depth_Decreased then
               Balance_2(L, Depth_Decreased);
            end if;
         end if;
      end Remove_Max;

   begin
      if From.Root /= null then
         Remove_Max(From.Root);
      else
         raise Bag_Empty;
      end if;
   end Remove_Max;


   generic
      with procedure Link_Action (L : in Link);
   procedure Link_Traversal (On_Bag : in Bag;
                             Order  : in Traversal_Order);

   procedure Link_Traversal (On_Bag : in Bag;
                             Order  : in Traversal_Order) is
   begin
      case Order is
         when Ascending =>
            declare

               procedure Traverse_Subtree (L : in Link) is

                  Z : Link := L;

               begin
                  while Z /= null loop
                     Traverse_Subtree(Z.Left);
                     Link_Action(Z);
                     Z := Z.Right;
                  end loop;
               end;

            begin
               Traverse_Subtree(On_Bag.Root);
            end;
         when Descending =>
            declare

               procedure Traverse_Subtree (L : in Link) is

                  Z : Link := L;

               begin
                  while Z /= null loop
                     Traverse_Subtree(Z.Right);
                     Link_Action(Z);
                     Z := Z.Left;
                  end loop;
               end;

            begin
               Traverse_Subtree(On_Bag.Root);
            end;
      end case;
   end Link_Traversal;


   procedure Traversal (On_Bag : in Bag;
                        Order  : in Traversal_Order := Ascending) is

      procedure Link_Action is new Action_On_Link(Action);

      procedure Traverse is new Link_Traversal(Link_Action);

   begin
      Traverse(On_Bag, Order);
   end Traversal;


   procedure Update_All (Within : in out Bag;
                         Order  : in Traversal_Order := Ascending) is

      procedure Link_Modify is new Modify_On_Link(Modify);

      procedure Update_All is new Link_Traversal(Link_Modify);

   begin
      Update_All(Within, Order);
   end Update_All;


   procedure Assign (Object : in out Bag; Value : in Bag) is

      function Copy_Of (Subtree : Link) return Link is
      begin
         if Subtree = null then
            return null;
         else
            declare

               L      : Link := Subtree.Right;
               Z      : Link := new Cell'(Val     => Subtree.Val,
                                          Balance => Subtree.Balance,
                                          Left    => Copy_Of(Subtree.Left),
                                          Right   => null);

               Result : constant Link := Z;

            begin
               while L /= null loop
                  Z.Right := new Cell'(Val     => L.Val,
                                       Balance => L.Balance,
                                       Left    => Copy_Of(Subtree => L.Left),
                                       Right   => null);
                  L := L.Right;
                  Z := Z.Right;
               end loop;
               return Result;
            end;
         end if;
      end Copy_Of;

   begin
      if Value.Duplicate_Keys_Allowed and not Object.Duplicate_Keys_Allowed then
         raise Constraint_Error;
      end if;
      Destroy(Object);
      Object.Root := Copy_Of(Subtree => Value.Root);
      Object.Card := Value.Card;
   end Assign;


   procedure Assign (Object : in out Bag; Value : in List) is
   begin
      if Object.Duplicate_Keys_Allowed then
         Destroy(Object);
         Insert(Value, Into => Object);
      else
         declare

            Temp : Bag(Duplicate_Keys_Allowed => False);

         begin
            begin
               Insert(Value, Into => Temp);
            exception
               when Duplicate_Key =>
                  Destroy(Temp);
                  raise;
            end;
            Destroy(Object);
            Object.Root := Temp.Root;
            Object.Card := Temp.Card;
         end;
      end if;
   end Assign;


   procedure Swap (Left, Right : in out Bag) is

      procedure Swap is new Exchange(Link);
      procedure Swap is new Exchange(Natural_Count);

   begin
      if Left.Duplicate_Keys_Allowed = Right.Duplicate_Keys_Allowed then
         Swap(Left.Root, Right.Root);
         Swap(Left.Card, Right.Card);
      elsif Left.Duplicate_Keys_Allowed then
         declare

            Temp : Bag(Duplicate_Keys_Allowed => False);

         begin
            Assign(Temp, Value => Left);
            Left.Root := Right.Root;
            Left.Card := Right.Card;
            Right.Root := Temp.Root;
            Right.Card := Temp.Card;
         end;
      elsif Right.Duplicate_Keys_Allowed then
         declare

            Temp : Bag(Duplicate_Keys_Allowed => False);

         begin
            Assign(Temp, Value => Right);
            Right.Root := Left.Root;
            Right.Card := Left.Card;
            Left.Root := Temp.Root;
            Left.Card := Temp.Card;
         end;
      end if;
   end Swap;


   procedure Destroy (The_Bag : in out Bag) is

      procedure No_Op (Item : in out Item_Type) is
      begin
         null;
      end;

      pragma Inline(No_Op);

      procedure Destroy is new Destruction(No_Op);

   begin
      Destroy(The_Bag);
   end Destroy;


   procedure Destruction (The_Bag : in out Bag) is

      procedure Delete_Subtree (L : in out Link) is

         Z : Link;

      begin
         while L /= null loop
            Delete_Subtree(L.Left);
            Destroy(L.Val);
            Z := L;
            L := L.Right;
            Dispose(Z);
         end loop;
      end Delete_Subtree;

   begin
      Delete_Subtree(The_Bag.Root);
      The_Bag.Card := 0;
   end Destruction;


   function To_List (From  : Bag;
                     Order : Traversal_Order := Ascending) return List is

      Result : List(1 .. From.Card);
      N      : Count range 0 .. Result'Last := 0;

      procedure Put_Into_Result (X : in Item_Type) is
      begin
         N := N + 1;
         Result(N) := X;
      end;

      pragma Inline(Put_Into_Result);

      procedure Build_List is new Traversal(Action => Put_Into_Result);

   begin
      Build_List(From, Order);
      return Result;
   end To_List;


   package body Implementation is separate;

end Bags;
