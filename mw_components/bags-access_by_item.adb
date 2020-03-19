-- GENERIC BAGS PACKAGE IMPLEMENTED WITH AVL TREES
   -----------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jul  1 17:33:03 1998
-- Update Count    : 11

-- Creation : 28-FEB-1989 by Mats Weber.


package body Bags.Access_By_Item is
--------------------------------

   use Implementation;


   function Pointer (X : Item_Type; L : Link) return Link is
      -- Returns a pointer to the first cell containing
      -- an item EQUAL to X in subtree L.
      -- Returns null if X is not found.

      function Pointer_To_X (L : Link) return Link is

         Z : Link := L;

      begin
         while Z /= null loop
            if Key_Of(X) < Key_Of(Z.Val) then
               Z := Z.Left;
            elsif Key_Of(X) = Key_Of(Z.Val) then
               declare

                  New_Z : constant Link := Pointer_To_X(Z.Left);

               begin
                  if New_Z = null then
                     if Equal(Z.Val, X) then
                        return Z;
                     else
                        Z := Z.Right;
                     end if;
                  else
                     return New_Z;
                  end if;
               end;
            else
               Z := Z.Right;
            end if;
         end loop;
         return null;
      end Pointer_To_X;

   begin
      return Pointer_To_X(L);
   end Pointer;

   pragma Inline(Pointer);


   procedure Remove (Item : in Item_Type; From : in out Bag) is

      Junk : Item_Type;

   begin
      Remove(Item, From, Removed_Item => Junk);
   end Remove;

   procedure Remove (Item : in Item_Type; From : in out Bag; Removed_Item : out Item_Type) is

      Depth_Decreased : Boolean := False;

      Item_Deleted : Boolean := False;

      -- copied from N.Wirth, "Algorithms + Data Structures = Programs", p 223

      procedure Remove (P : in out Link) is
      begin
         if P /= null then
            if Key_Of(Item) < Key_Of(P.Val) then
               -- delete in left subtree
               Remove(P.Left);
               if Depth_Decreased then
                  Balance_1(P, Depth_Decreased);
               end if;
            elsif Key_Of(Item) = Key_Of(P.Val) then
               if From.Duplicate_Keys_Allowed then
                  -- try deleting in left subtree
                  Remove(P.Left);
                  if Depth_Decreased then
                     Balance_1(P, Depth_Decreased);
                  end if;
               end if;
               if not Item_Deleted then
                  if Equal(Item, P.Val) then
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
                     Item_Deleted := True;
                  elsif From.Duplicate_Keys_Allowed then
                     -- try deleting in right subtree
                     Remove(P.Right);
                     if Depth_Decreased then
                        Balance_2(P, Depth_Decreased);
                     end if;
                  end if;
               end if;
            else
               -- delete in right subtree
               Remove(P.Right);
               if Depth_Decreased then
                  Balance_2(P, Depth_Decreased);
               end if;
            end if;
         end if;
      end Remove;

   begin
      Remove(From.Root);
      if not Item_Deleted then
         raise Nonexistent_Item;
      end if;
   end Remove;


   function Search (Item : Item_Type; Within : Bag) return Item_Type is

      Z : constant Link := Pointer(Item, Within.Root);

   begin
      if Z /= null then
         return Z.Val;
      else
         raise Nonexistent_Item;
      end if;
   end Search;

   function Search (Item : Item_Type; Within : Bag) return List is

      function Equiv (L : Link) return List is
      begin
         if L = null then
            declare

               Null_List : List(1..0);

            begin
               return Null_List;
            end;
         else
            if Key_Of(Item) < Key_Of(L.Val) then
               return Equiv(L.Left);
            elsif Key_Of(Item) = Key_Of(L.Val) then
               if Equal(Item, L.Val) then
                  return Equiv(L.Left) & L.Val & Equiv(L.Right);
               else
                  return Equiv(L.Left) & Equiv(L.Right);
               end if;
            else
               return Equiv(L.Right);
            end if;
         end if;
      end Equiv;

   begin
      return Equiv(Pointer(Key_Of(Item), Within.Root, First_Match => True));
   end Search;


   procedure Update (Item : in Item_Type; Within : in out Bag) is

      Z        : constant Link := Pointer(Item, Within.Root);
      Old_Item : Item_Type;

   begin
      if Z = null then
         raise Nonexistent_Item;
      else
         Old_Item := Z.Val;
         Modify(Z.Val);
         if Key_Of(Old_Item) /= Key_Of(Z.Val) then
            Z.Val := Old_Item;
            raise Invalid_Key;
         end if;
      end if;
   end Update;


   procedure Replace (Item     : in Item_Type;
                      New_Item : in Item_Type;
                      Within   : in out Bag) is

      Z : constant Link := Pointer(Item, Within.Root);

   begin
      if Z = null then
         raise Nonexistent_Item;
      elsif Key_Of(New_Item) /= Key_Of(Z.Val) then
         raise Invalid_Key;
      else
         Z.Val := New_Item;
      end if;
   end Replace;


   function Member (Item : Item_Type; Of_Bag : Bag) return Boolean is
   begin
      return Pointer(Item, Of_Bag.Root) /= null;
   end Member;

end Bags.Access_By_Item;
