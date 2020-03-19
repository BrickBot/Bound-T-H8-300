-- Filename        : test_protected_bags.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : long ago
-- Last Modified By: Mats Weber
-- Last Modified On: Fri Jul  3 18:58:27 1998
-- Update Count    : 20


with Protected_Bags.Access_By_Item,
     Protected_Bags.Bounded_Operations,
     Protected_Bags.Set_Operations,
     Read_Write_Semaphore,
     Text_IO,
     Integer_Text_IO,
     User_Interface,
     Min_Max_Functions,
     Standard_Random,
     Quick_Sort,
     Exchange,
     Number_Images,
     Unchecked_Conversion;

use Text_IO,
    Integer_Text_IO,
    Standard_Random,
    User_Interface;

procedure Test_Protected_Bags is
-----------------------------

   Test_Error : exception;


   type Rec is
      record
         Key           : Integer;
         Field         : Integer;
         Serial_Number : Positive;
      end record;

   function Key_Of (X : Rec) return Integer;

   function Same_Key_And_Field (X, Y : Rec) return Boolean;

   package Rec_Bags is new Protected_Bags(Key_Type  => Integer,
                                          Item_Type => Rec,
                                          Key_Of    => Key_Of,
                                          Count     => Integer);

   use Rec_Bags;


   package Rec_Bags_Access_By_Item is
      new Rec_Bags.Access_By_Item(Equal => Same_Key_And_Field);

   use Rec_Bags_Access_By_Item;


   package Rec_Bags_Bounded_Operations is new Rec_Bags.Bounded_Operations;
   use Rec_Bags_Bounded_Operations;

   package Rec_Bags_Set_Operations is new Rec_Bags.Set_Operations;
   use Rec_Bags_Set_Operations;


   function Image is new Number_Images.Integer_Image(Integer);


   type Cell;

   type Link is access Cell;

   type Equilibrium is range -1 .. +1;

   type Cell is
      record
         Val         : Rec;
         Balance     : Equilibrium;
         Left, Right : Link;
      end record;

   type Unprotected_Binary_Tree (Duplicate_Keys_Allowed : Boolean) is limited
      record
         Root : Link;
         Card : Natural;
      end record;

   type Binary_Tree (Duplicate_Keys_Allowed : Boolean) is
      record
         The_Bag : Unprotected_Binary_Tree(Duplicate_Keys_Allowed);
         Lock    : Read_Write_Semaphore.Semaphore;
      end record;

   function To_Binary_Tree is new Unchecked_Conversion(Source => Bag,
                                                       Target => Binary_Tree);


   function Key_Of (X : Rec) return Integer is
   begin
      return X.Key;
   end;


   function Same_Key_And_Field (X, Y : Rec) return Boolean is
   begin
      return X.Key = Y.Key and X.Field = Y.Field;
   end Same_Key_And_Field;


   function Image (X : Rec) return String is
   begin
      return "(" & Image(X.Key) & "," &
                   Image(X.Field) & "," &
                   Image(X.Serial_Number) & ")";
   end Image;


   procedure Check (Condition : in Boolean) is
   begin
      if not Condition then
         raise Test_Error;
      end if;
   end Check;


   procedure Put_Structure (T : in Bag) is

      procedure Put_Subtree (L : in Link; Level : in Positive) is
      begin
         if L /= null then
            Put_Subtree(L.Left,  Level + 1);
            Put_Line((1 .. 5 * (Level - 1) => ' ') & Image(L.Val));
            Put_Subtree(L.Right, Level + 1);
         end if;
      end Put_Subtree;

   begin
      Put_Subtree(To_Binary_Tree(T).The_Bag.Root, Level => 1);
   end Put_Structure;


   procedure Check_Balance (T : in Bag; Maximum_Depth : out Natural) is

      Balance_Error : exception;


      procedure Check_Balance (L : in Link; Max_Depth : out Natural) is

         Max_Left_Depth,
         Max_Right_Depth : Natural;

         function Max is new Min_Max_Functions.Maximum(Integer);

      begin
         if L /= null then
            Check_Balance(L.Left,  Max_Left_Depth);
            Check_Balance(L.Right, Max_Right_Depth);
            if abs (Max_Right_Depth - Max_Left_Depth) > 1 or else
               Equilibrium(Max_Right_Depth - Max_Left_Depth) /= L.Balance
            then
               raise Balance_Error;
            end if;
            Max_Depth := 1 + Max(Max_Left_Depth, Max_Right_Depth);
         else
            Max_Depth := 0;
         end if;
      end Check_Balance;

   begin
      Check_Balance(To_Binary_Tree(T).The_Bag.Root, Max_Depth => Maximum_Depth);
   exception
      when Balance_Error =>
         Put_Line("   THE TREE IS NOT BALANCED PROPERLY");
         Put_Line("   ---------------------------------");
   end Check_Balance;


   procedure Random_Test is

      Max_Depth_Of_B : Natural;

      type Order is (Ascending, Descending, Random);


      function Lt (L, R : Rec) return Boolean is
      begin
         if L.Key /= R.Key then
            return L.Key < R.Key;
         elsif L.Field /= R.Field then
            return L.Field < R.Field;
         else
            return L.Serial_Number < R.Serial_Number;
         end if;
      end Lt;

      function Gt (L, R : Rec) return Boolean is
      begin
         if L.Key /= R.Key then
            return L.Key > R.Key;
         elsif L.Field /= R.Field then
            return L.Field > R.Field;
         else
            return L.Serial_Number > R.Serial_Number;
         end if;
      end Gt;

      procedure Sort_Ascending  is new Quick_Sort(Positive, Rec, List, Lt);
      procedure Sort_Descending is new Quick_Sort(Positive, Rec, List, Gt);

      procedure Swap is new Exchange(Rec);


      function Natural_Answer is
         new User_Interface.Integer_Answer(Natural);

      function Order_Answer (Prompt : String) return Order is

         Ch : Character;

      begin
         loop
            Ch := String_Answer(Prompt)(1);
            case Ch is
               when 'a' | 'A' =>
                  return Ascending;
               when 'd' | 'D' =>
                  return Descending;
               when 'r' | 'R' =>
                  return Random;
               when others =>
                  null;
            end case;
         end loop;
      end Order_Answer;

   begin
      loop
         declare

            B : Bag(Duplicate_Keys_Allowed => Yes_No_Answer("Allow duplicate keys ? "));

            Duplicates : Bag(Duplicate_Keys_Allowed => True);

            Items : List(1 .. Natural_Answer("Number of items : ")) :=
                    (others =>
                        (Key           => Uniform(1, 1000),
                         Field         => Uniform(1, 1000),
                         Serial_Number => Uniform(1, 1000)));

         begin
            Initialize(B, Priority_To => Read_Write_Semaphore.Writers);
            Initialize(Duplicates, Priority_To => Read_Write_Semaphore.Writers);
            case Order_Answer("Insertion order (a | d | r) : ") is
               when Ascending =>
                  Sort_Ascending(Items);
               when Descending =>
                  Sort_Descending(Items);
               when Random =>
                  null;
            end case;

            for I in Items'Range loop
               declare

                  Previous_B : Bag(B.Duplicate_Keys_Allowed);

               begin
                  Initialize(Previous_B);
                  Assign(Previous_B, Value => B);
                  begin
                     Insert(Items(I), Into => B);
                  exception
                     when Duplicate_Key =>
                        Insert(Items(I), Into => Duplicates);
                        Check(not B.Duplicate_Keys_Allowed);
                  end;
                  Check_Balance(B, Maximum_Depth => Max_Depth_Of_B);
                  Check(B = B and B <= B);
                  if Card(Previous_B) < Card(B) then
                     if B.Duplicate_Keys_Allowed then
                        Check(not (Previous_B > B));
                        Check(Previous_B <= B);
                     else
                        Check(Previous_B < B);
                        Check(not (Previous_B > B));
                        Check(Previous_B <= B);
                        Check(not (Previous_B >= B));
                        Check(Previous_B /= B);
                     end if;
                  elsif Card(Previous_B) = Card(B) then
                     -- This was a duplicate key.
                     Check(not B.Duplicate_Keys_Allowed);
                     Check(not (Previous_B < B));
                     Check(not (Previous_B > B));
                     Check(Previous_B <= B);
                     Check(Previous_B >= B);
                     Check(Previous_B = B);
                  else
                     raise Test_Error;
                  end if;
                  Destroy(Previous_B);
               end;

               if B.Duplicate_Keys_Allowed then
                  declare

                     B_As_List : List := To_List(B);

                  begin
                     Sort_Ascending(B_As_List);
                     Sort_Ascending(Items(1 .. I));
                     Check(B_As_List = Items(1 .. I));
                     B_As_List := To_List(B, Order => Descending);
                     Sort_Descending(B_As_List);
                     Sort_Descending(Items(1 .. I));
                     Check(B_As_List = Items(1 .. I));
                  end;
               end if;
            end loop;

            Put_Line("Card(B) = " & Image(Card(B)) & ", Max depth = " & Image(Max_Depth_Of_B));

            if B.Duplicate_Keys_Allowed then
               Check(Empty(Duplicates));
            end if;

            case Order_Answer("Removal order (a | d | r) : ") is
               when Ascending =>
                  Sort_Ascending(Items);
               when Descending =>
                  Sort_Descending(Items);
               when Random =>
                  -- randomly shuffle the items
                  for J in reverse Items'First + 1 .. Items'Last loop
                     Swap(Items(J), Items(Uniform(Items'First, J)));
                  end loop;
            end case;

            for I in Items'Range loop
               declare

                  Previous_B : Bag(B.Duplicate_Keys_Allowed);

               begin
                  Initialize(Previous_B);
                  Assign(Previous_B, Value => B);
                  begin
                     Remove(Item => Items(I), From => B);
                  exception
                     when Nonexistent_Item =>
                        Check(Member(Item => Items(I), Of_Bag => Duplicates));
                        Check(not B.Duplicate_Keys_Allowed);
                  end;
                  Check_Balance(B, Maximum_Depth => Max_Depth_Of_B);
                  Check(B = B and B <= B);
                  if Card(Previous_B) < Card(B) then
                     raise Test_Error;
                  elsif Card(Previous_B) = Card(B) then
                     -- This was a duplicate key.
                     Check(not B.Duplicate_Keys_Allowed);
                     Check(not (Previous_B < B));
                     Check(not (Previous_B > B));
                     Check(Previous_B <= B);
                     Check(Previous_B >= B);
                     Check(Previous_B = B);
                  else
                     if B.Duplicate_Keys_Allowed then
                        Check(not (Previous_B < B));
                        Check(Previous_B >= B);
                     else
                        Check(not (Previous_B < B));
                        Check(Previous_B > B);
                        Check(not (Previous_B <= B));
                        Check(Previous_B >= B);
                        Check(Previous_B /= B);
                     end if;
                  end if;
                  Destroy(Previous_B);
               end;
            end loop;

            Destroy(Duplicates);
            Check(Empty(B));
         end;
         exit when not Yes_No_Answer("More random tests ? ");
      end loop;
   end Random_Test;


   procedure Put (L : in List) is
   begin
      for I in L'Range loop
         Put(Image(L(I)));
         if I /= L'Last then
            Put(", ");
         end if;
      end loop;
   end Put;

   procedure Put (T : in Bag) is
   begin
      Put_Line("Cardinal : " & Image(Card(T)));
      Put("Contents : ");
      Put(To_List(T));
      New_Line;
   end Put;

   function Int_Answer is new Integer_Answer(Integer);


begin
   if Yes_No_Answer("Perform random tests ? ") then
      Random_Test;
   end if;
   --
   declare

      T : Bag(Duplicate_Keys_Allowed => Yes_No_Answer("Allow duplicate keys ? "));

      Max_Depth_Of_T : Natural;

      X : Integer;
      F : Integer;

      Ch : Character;

      Field_Present : Boolean;

      Current_Serial_Number : Natural := 0;


      function New_Serial_Number return Positive is
      begin
         Current_Serial_Number := Current_Serial_Number + 1;
         return Current_Serial_Number;
      end New_Serial_Number;

      procedure Get_Parameters (X         : out Integer;
                                F         : out Integer;
                                F_Present : out Boolean) is

         Ch : Character;

      begin
         Get(X);
         F := 0;
         F_Present := False;
         while not End_Of_Line loop
            Get(Ch);
            if Ch = ',' then
               Get(F);
               F_Present := True;
               exit;
            end if;
         end loop;
         Skip_Line;
         New_Line;
      end Get_Parameters;

   begin
      Initialize(T);
      Main_Loop :
         loop
            New_Line;
            loop
               Put("(Insert | Remove | Search) <element> [, <field>] | List | Terminate : ");
               Get(Ch);
               case Ch is
                  when 'I' | 'i' =>
                     Get_Parameters(X, F, Field_Present);
                     if Field_Present then
                        begin
                           Insert(Item => (Key => X, Field => F, Serial_Number => New_Serial_Number), Into => T);
                           exit;
                        exception
                           when Duplicate_Key =>
                              Put_Line("   Duplicate key");
                        end;
                     else
                        begin
                           Insert(Item => (Key => X, Field => 0, Serial_Number => New_Serial_Number), Into => T);
                           exit;
                        exception
                           when Duplicate_Key =>
                              Put_Line("   Duplicate key");
                        end;
                     end if;
                  when 'R' | 'r' =>
                     Get_Parameters(X, F, Field_Present);
                     if Field_Present then
                        begin
                           Remove(Item => (Key => X, Field => F, Serial_Number => Positive'Last), From => T);
                           exit;
                        exception
                           when Nonexistent_Item =>
                              Put_Line("   Nonexistent item");
                        end;
                     else
                        begin
                           Remove(Key => X, From => T);
                           exit;
                        exception
                           when Nonexistent_Key =>
                              Put_Line("   Nonexistent key");
                        end;
                     end if;
                  when 'S' | 's' =>
                     Get_Parameters(X, F, Field_Present);
                     if Field_Present then
                        begin
                           Put_Line("   " & Image(Search(Item => (X, F, Positive'Last), Within => T)));
                        exception
                           when Nonexistent_Item =>
                              Put_Line("   Nonexistent item");
                        end;
                        declare

                           Items : constant List := Search(Item => (X, F, Positive'Last), Within => T);

                        begin
                           Put("   ");
                           Put(Items);
                           if Items'Length = 0 then
                              Put("-- null list --");
                           end if;
                           New_Line;
                        end;
                     else
                        begin
                           Put_Line("   " & Image(Search(Key => X, Within => T)));
                        exception
                           when Nonexistent_Key =>
                              Put_Line("   Nonexistent key");
                        end;
                        declare

                           Items : constant List := Search(Key => X, Within => T);

                        begin
                           Put("   ");
                           Put(Items);
                           if Items'Length = 0 then
                              Put("-- null list --");
                           end if;
                           New_Line;
                        end;
                     end if;
                  when 'L' | 'l' =>
                     Skip_Line;
                     declare

                        Lower_Bounded : constant Boolean := Yes_No_Answer("   Lower Bounded ? ");
                        Upper_Bounded : constant Boolean := Yes_No_Answer("   Upper Bounded ? ");

                        Lower_Bound,
                        Upper_Bound   : Integer;

                        Direction     : Traversal_Order;

                     begin
                        if Lower_Bounded then
                           Lower_Bound := Int_Answer("   Lower Bound : ");
                        end if;
                        if Upper_Bounded then
                           Upper_Bound := Int_Answer("   Upper Bound : ");
                        end if;
                        if Yes_No_Answer("   Traverse in descending order ? ") then
                           Direction := Descending;
                        else
                           Direction := Ascending;
                        end if;
                        Put("   ");
                        if Upper_Bounded then
                           if Lower_Bounded then
                              Put(Bounded_List(T, First => Lower_Bound, Last => Upper_Bound, Order => Direction));
                           else
                              Put(Upper_Bounded_List(T, Last => Upper_Bound, Order => Direction));
                           end if;
                        else
                           if Lower_Bounded then
                              Put(Lower_Bounded_List(T, First => Lower_Bound, Order => Direction));
                           else
                              Put(To_List(T, Order => Direction));
                           end if;
                        end if;
                        New_Line;
                     end;
                  when 'T' | 't' =>
                     Skip_Line;
                     New_Line;
                     exit Main_Loop;
                  when others =>
                     Skip_Line;
                     New_Line;
               end case;
            end loop;
            Put_Structure(T);
            Put_Line("Cardinal : " & Image(Card(T)));
            Check_Balance(T, Max_Depth_Of_T);
            Put_Line("Maximum depth : " & Image(Max_Depth_Of_T));
         end loop Main_Loop;
      Destroy(T);
      Put_Structure(T);
   end;
end Test_Protected_Bags;
