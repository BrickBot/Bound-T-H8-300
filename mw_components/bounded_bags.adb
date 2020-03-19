-- Generic Bags package based on sorted arrays and binary search
   -------------------------------------------------------------

-- Creation :  3-JUL-1989 by Mats Weber.


with Binary_Search;

package body Bounded_Bags is
-------------------------

   package Searching is new Binary_Search(Item_Type  => Item_Type,
                                          Key_Type   => Key_Type,
                                          Index      => Positive_Count,
                                          Item_Array => Item_Array,
                                          Key_Of     => Key_Of);

   use Searching;


   procedure Insert (Item : in Item_Type; Into : in out Bag) is

      Item_Location : constant Searching.Location :=
                      Locate(Key    => Key_Of(Item),
                             Within => Into.Contents(1..Into.Last));

   begin
      case Item_Location.Kind is
         when Found =>
            if not Into.Duplicate_Keys_Allowed then
               raise Duplicate_Key;
            end if;
            Into.Contents(Item_Location.Position + 1..Into.Last + 1) :=
               Into.Contents(Item_Location.Position..Into.Last);
            Into.Contents(Item_Location.Position) := Item;
            Into.Last := Into.Last + 1;
         when Inbetween =>
            Into.Contents(Item_Location.After + 1..Into.Last + 1) :=
               Into.Contents(Item_Location.After..Into.Last);
            Into.Contents(Item_Location.After) := Item;
            Into.Last := Into.Last + 1;
         when Smaller =>
            Into.Contents(2..Into.Last + 1) := Into.Contents(1..Into.Last);
            Into.Contents(1) := Item;
            Into.Last := Into.Last + 1;
         when Greater =>
            Into.Contents(Into.Last + 1) := Item;
            Into.Last := Into.Last + 1;
         when Null_Array =>
            Into.Contents(1) := Item;
            Into.Last := 1;
      end case;
   end Insert;


   procedure Remove (Key : in Key_Type; From : in out Bag) is
   begin
      declare

         Item_Location : constant Positive_Count range 1..From.Last :=
                         Locate(Key    => Key,
                                Within => From.Contents(1..From.Last));

      begin
         From.Contents(Item_Location..From.Last - 1) :=
            From.Contents(Item_Location + 1..From.Last);
         From.Last := From.Last - 1;
      end;
   exception
      when Searching.Not_Found =>
         raise Nonexistent_Key;
   end Remove;


   function Search (Key : Key_Type; Within : Bag) return Item_Type is
   begin
      return Within.Contents
                (Locate(Key    => Key,
                        Within => Within.Contents(1..Within.Last)));
   exception
      when Searching.Not_Found =>
         raise Nonexistent_Key;
   end Search;


   function Card (Of_Bag : Bag) return Natural_Count is
   begin
      return Of_Bag.Last;
   end;


   function Empty (The_Bag : Bag) return Boolean is
   begin
      return The_Bag.Last = 0;
   end;

end Bounded_Bags;
