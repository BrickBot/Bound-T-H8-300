-- Filename        : test_lists.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : long ago
-- Last Modified By: Mats Weber
-- Last Modified On: Thu May 28 19:04:57 1998
-- Update Count    : 4


with Lists;

procedure Test_Lists is

   package Integer_Lists is new Lists(Integer,
                                      Count => Natural);

   use Integer_Lists;

   type Integer_Array is array (Positive range <>) of Integer;


   L1, L2, L3, L4 : List;
   P1, P2         : Pointer;
   I1             : Integer;


   Test_Error : exception;


   procedure Check (Condition : in Boolean) is
   begin
      if not Condition then
         raise Test_Error;
      end if;
   end Check;


   procedure Check (The_List          : in List;
                    Expected_Contents : in Integer_Array) is

      N : Natural range Expected_Contents'First - 1 .. Expected_Contents'Last :=
          Expected_Contents'First - 1;

      procedure Check_Element (K : in Integer) is
      begin
         N := N + 1;
         Check(Expected_Contents(N) = K);
      exception
         when Constraint_Error =>
            raise Test_Error;
      end Check_Element;

      procedure Check_Element_Reverse (K : in Integer) is
      begin
         Check(Expected_Contents(N) = K);
         N := N - 1;
      exception
         when Constraint_Error =>
            raise Test_Error;
      end Check_Element_Reverse;

      procedure Check_All         is new Traversal(Check_Element);
      procedure Check_All_Reverse is new Traversal(Check_Element_Reverse);

   begin
      Check(Length(The_List) = Expected_Contents'Length);
      Check_All(The_List);
      Check(N = Expected_Contents'Last);
      Check_All_Reverse(The_List, Direction => Backward);
      Check(N = Expected_Contents'First - 1);
   end Check;


   function "=" is new Integer_Lists.Equality;

begin
   -- Test insertions into lists of length 0
   Check(Length(L1) = 0);
   Destroy(L1);
   Check(Length(L1) = 0);
   Check(Tail(L1) = Null_Pointer and Head(L1) = Null_Pointer);
   Insert_At_Tail(Item => 1, Into => L1);
   Check(Length(L1) = 1);
   Check(Tail(L1) = Head(L1));
   Check(Element(Tail(L1)) = 1);
   Check(Element(Head(L1)) = 1);
   Destroy(L1);
   Check(Length(L1) = 0);
   Insert_Before(Item => 2, Into => L1, Before => Tail(L1));
   Check(Length(L1) = 1);
   Check(Tail(L1) = Head(L1));
   Check(Element(Tail(L1)) = 2);
   Check(Element(Head(L1)) = 2);
   begin
      Insert_Before(Item => 2, Into => L1, Before => Null_Pointer);
      raise Test_Error;
   exception
      when Pointer_Is_Null =>
         null;
   end;
   Destroy(L1);
   Check(Length(L1) = 0);
   Insert_At_Head(Item => 1, Into => L1);
   Check(Length(L1) = 1);
   Check(Tail(L1) = Head(L1));
   Check(Element(Tail(L1)) = 1);
   Check(Element(Head(L1)) = 1);
   Destroy(L1);
   Check(Length(L1) = 0);
   Insert_After(Item => 2, Into => L1, After => Head(L1));
   Check(Length(L1) = 1);
   Check(Tail(L1) = Head(L1));
   Check(Element(Tail(L1)) = 2);
   Check(Element(Head(L1)) = 2);
   begin
      Insert_After(Item => 2, Into => L1, After => Null_Pointer);
      raise Test_Error;
   exception
      when Pointer_Is_Null =>
         null;
   end;
   Destroy(L1);
   Check(Length(L1) = 0);
   -- Test MOVE procedures on short lists
   Move_Before(From => L1, Into => L2, Before => Tail(L2));
   Check(Length(L1) = 0 and Length(L2) = 0);
   Move_After(From => L1, Into => L2, After => Head(L2));
   Check(Length(L1) = 0 and Length(L2) = 0);
   Insert_At_Head(Item => 1, Into => L2);
   Move_Before(From => L2, Into => L1, Before => Tail(L1));
   Check(Length(L1) = 1 and Length(L2) = 0);
   Move_After(From => L1, Into => L2, After => Head(L2));
   Check(Length(L1) = 0 and Length(L2) = 1);
   Destroy(L2);
   -- Test insertion and move on longer lists
   Insert_At_Head(Item => 1, Into => L1);
   Check(The_List => L1, Expected_Contents => (1 => 1));
   Insert_At_Head(Item => 2, Into => L1);
   Check(The_List => L1, Expected_Contents => (2, 1));
   Insert_At_Head(Item => 3, Into => L1);
   Check(The_List => L1, Expected_Contents => (3, 2, 1));
   Insert_At_Head(Item => 4, Into => L1);
   Check(The_List => L1, Expected_Contents => (4, 3, 2, 1));
   Insert_At_Head(Item => 5, Into => L1);
   Check(The_List => L1, Expected_Contents => (5, 4, 3, 2, 1));
   Insert_At_Head(Item => 6, Into => L1);
   Check(The_List => L1, Expected_Contents => (6, 5, 4, 3, 2, 1));
   Insert_At_Head(Item => 7, Into => L1);
   Check(The_List => L1, Expected_Contents => (7, 6, 5, 4, 3, 2, 1));
   Insert_At_Head(Item => 8, Into => L1);
   Check(The_List => L1, Expected_Contents => (8, 7, 6, 5, 4, 3, 2, 1));
   Insert_At_Head(Item => 9, Into => L1);
   Check(The_List => L1, Expected_Contents => (9, 8, 7, 6, 5, 4, 3, 2, 1));
   Insert_At_Tail(Item => 0, Into => L1);
   Check(The_List => L1, Expected_Contents => (9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
   Insert_At_Tail(Item => 11, Into => L2);
   Check(The_List => L2, Expected_Contents => (1 => 11));
   Insert_At_Tail(Item => 12, Into => L2);
   Check(The_List => L2, Expected_Contents => (11, 12));
   Insert_At_Tail(Item => 15, Into => L2);
   Check(The_List => L2, Expected_Contents => (11, 12, 15));
   Insert_At_Head(Item => 10, Into => L2);
   Check(The_List => L2, Expected_Contents => (10, 11, 12, 15));
   Insert_After(Item => 13, Into => L2, After => Next(Next(Head(L2))));
   Check(The_List => L2, Expected_Contents => (10, 11, 12, 13, 15));
   Insert_After(Item => 16, Into => L2, After => Tail(L2));
   Check(The_List => L2, Expected_Contents => (10, 11, 12, 13, 15, 16));
   Insert_Before(Item => 14, Into => L2, Before => Previous(Previous(Next(Tail(L2)))));
   Check(The_List => L2, Expected_Contents => (10, 11, 12, 13, 14, 15, 16));
   Check(L3 = L4);
   Assign(L4, Value => L1);
   Check(L1 = L4);
   Check(L3 /= L2);
   Check(The_List => L4, Expected_Contents => (9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
   Assign(L3, Value => L4);
   Check(L1 = L3);
   Check(L4 /= L2);
   Check(The_List => L3, Expected_Contents => (9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
   Assign(L4, Value => L2);
   Check(The_List => L4, Expected_Contents => (10, 11, 12, 13, 14, 15, 16));
   Swap(L1, L2);
   Check(The_List => L1, Expected_Contents => (10, 11, 12, 13, 14, 15, 16));
   Check(The_List => L2, Expected_Contents => (9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
   Move_After(From => L1, Into => L2, After => Next(Next(Next(Head(L2)))));
   Check(Length(L1) = 0);
   Check(The_List => L2,
         Expected_Contents => (9, 8, 7, 6, 10, 11, 12, 13, 14, 15, 16, 5, 4, 3, 2, 1, 0));
   Move_After(From => L2, Into => L1, After => Tail(L1));
   Check(Length(L2) = 0);
   Check(The_List => L1,
         Expected_Contents => (9, 8, 7, 6, 10, 11, 12, 13, 14, 15, 16, 5, 4, 3, 2, 1, 0));
   Swap(L1, L2);
   Check(Length(L1) = 0);
   Check(The_List => L2,
         Expected_Contents => (9, 8, 7, 6, 10, 11, 12, 13, 14, 15, 16, 5, 4, 3, 2, 1, 0));
   begin
      Move_After(From => L3, Into => L4, After => Null_Pointer);
      raise Test_Error;
   exception
      when Pointer_Is_Null =>
         null;
   end;
   Check(The_List => L3, Expected_Contents => (9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
   Check(The_List => L4, Expected_Contents => (10, 11, 12, 13, 14, 15, 16));
   begin
      Move_Before(From => L3, Into => L4, Before => Null_Pointer);
      raise Test_Error;
   exception
      when Pointer_Is_Null =>
         null;
   end;
   Check(The_List => L3, Expected_Contents => (9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
   Check(The_List => L4, Expected_Contents => (10, 11, 12, 13, 14, 15, 16));
   -- Test removal
   Remove_Head(From => L2, Removed_Item => I1);
   Check(I1 = 9);
   Check(The_List => L2,
         Expected_Contents => (8, 7, 6, 10, 11, 12, 13, 14, 15, 16, 5, 4, 3, 2, 1, 0));
   Remove_Tail(From => L2, Removed_Item => I1);
   Check(I1 = 0);
   Check(The_List => L2, Expected_Contents => (8, 7, 6, 10, 11, 12, 13, 14, 15, 16, 5, 4, 3, 2, 1));
   P1 := Next(Head(L2));
   Remove(From => L2, Position => P1, Removed_Item => I1);
   Check(I1 = 7);
   Check(The_List => L2, Expected_Contents => (8, 6, 10, 11, 12, 13, 14, 15, 16, 5, 4, 3, 2, 1));
   Check(Element(P1) = 6);
   Check(Next(Head(L2)) = P1);
   P1 := Tail(L2);
   Remove(From => L2, Position => P1, Removed_Item => I1, Direction => Backward);
   Check(I1 = 1);
   Check(The_List => L2, Expected_Contents => (8, 6, 10, 11, 12, 13, 14, 15, 16, 5, 4, 3, 2));
   Check(Element(P1) = 2);
   Check(Tail(L2) = P1);
   declare

      Contents_Of_L2 : constant Integer_Array := (8, 6, 10, 11, 12, 13, 14, 15, 16, 5, 4, 3, 2);
      First          : Natural range Contents_Of_L2'First .. Contents_Of_L2'Last + 1 :=
                       Contents_Of_L2'First;
      Last           : Natural range Contents_Of_L2'First - 1 .. Contents_Of_L2'Last :=
                       Contents_Of_L2'Last;

   begin
      Assign(L1, Value => L2);
      P1 := Head(L2);
      while P1 /= Null_Pointer loop
         Remove(From => L2, Position => P1, Removed_Item => I1);
         Check(I1 = Contents_Of_L2(First));
         First := First + 1;
         Check(The_List => L2, Expected_Contents => Contents_Of_L2(First .. Contents_Of_L2'Last));
      end loop;
      First := Contents_Of_L2'First;
      Assign(L2, Value => L1);
      while Length(L2) > 0 loop
         Remove_Head(From => L2, Removed_Item => I1);
         Check(I1 = Contents_Of_L2(First));
         First := First + 1;
         Check(The_List => L2, Expected_Contents => Contents_Of_L2(First .. Contents_Of_L2'Last));
      end loop;
      First := Contents_Of_L2'First;
      Assign(L2, Value => L1);
      P1 := Tail(L2);
      while P1 /= Null_Pointer loop
         Remove(From => L2, Position => P1, Removed_Item => I1, Direction => Backward);
         Check(I1 = Contents_Of_L2(Last));
         Last := Last - 1;
         Check(The_List => L2, Expected_Contents => Contents_Of_L2(Contents_Of_L2'First .. Last));
      end loop;
      Last := Contents_Of_L2'Last;
      Assign(L2, Value => L1);
      while Length(L2) > 0 loop
         Remove_Tail(From => L2, Removed_Item => I1);
         Check(I1 = Contents_Of_L2(Last));
         Last := Last - 1;
         Check(The_List => L2, Expected_Contents => Contents_Of_L2(Contents_Of_L2'First .. Last));
      end loop;
      Last := Contents_Of_L2'Last;
      Assign(L2, Value => L1);
      loop
         exit when Empty(L2);
         Remove_Head(From => L2, Removed_Item => I1);
         Check(I1 = Contents_Of_L2(First));
         First := First + 1;
         Check(The_List => L2, Expected_Contents => Contents_Of_L2(First .. Last));
         exit when Empty(L2);
         Remove_Tail(From => L2, Removed_Item => I1);
         Check(I1 = Contents_Of_L2(Last));
         Last := Last - 1;
         Check(The_List => L2, Expected_Contents => Contents_Of_L2(First .. Last));
      end loop;
      First := Contents_Of_L2'First;
      Last  := Contents_Of_L2'Last;
      Assign(L2, Value => L1);
      P1 := Head(L2);
      P2 := Tail(L2);
      loop
         exit when Empty(L2);
         Remove(From => L2, Position => P1, Removed_Item => I1);
         Check(I1 = Contents_Of_L2(First));
         First := First + 1;
         Check(The_List => L2, Expected_Contents => Contents_Of_L2(First .. Last));
         exit when Empty(L2);
         Remove(From => L2, Position => P2, Removed_Item => I1, Direction => Backward);
         Check(I1 = Contents_Of_L2(Last));
         Last := Last - 1;
         Check(The_List => L2, Expected_Contents => Contents_Of_L2(First .. Last));
      end loop;
      First := Contents_Of_L2'First;
      Last  := Contents_Of_L2'Last;
      Assign(L2, Value => L1);
   end;
   Destroy(L1);
   Destroy(L2);
   Destroy(L3);
   Destroy(L4);
end Test_Lists;
