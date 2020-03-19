-- GENERIC PACKAGE FOR SETS OF ANY BASE TYPE
   -----------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jul  1 20:22:42 1998
-- Update Count    : 9

-- Creation : 20-JUN-1985 by S. Mourtada.


with Bags.Set_Operations;

package body General_Sets is
-------------------------

   package Element_Bags_Set_Operations is new Element_Bags.Set_Operations;


   function Identity (X : Element_Type) return Element_Type is
   begin
      return X;
   end;


   function "<=" (Left : Set; Right : Set) return Boolean is
   begin
      return Element_Bags_Set_Operations."<="(Element_Bags.Bag(Left),
                                              Element_Bags.Bag(Right));
   end "<=";

   function "<=" (Left : Set; Right : List) return Boolean is

      Result     : Boolean;
      Right_Set  : Set;

   begin
      Add(Right, To => Right_Set);
      Result := Left <= Right_Set;
      Empty(Right_Set);
      return Result;
   end "<=";

   function "<=" (Left : List; Right : Set) return Boolean is
   begin
      for I in Left'Range loop
         if not Member(Left(I), Right) then
            return False;
         end if;
      end loop;
      return True;
   end "<=";

   function ">=" (Left : Set; Right : Set) return Boolean is
   begin
      return Element_Bags_Set_Operations.">="(Element_Bags.Bag(Left),
                                              Element_Bags.Bag(Right));
   end ">=";

   function ">=" (Left : Set; Right : List) return Boolean is
   begin
      return Right <= Left;
   end ">=";

   function ">=" (Left : List; Right : Set) return Boolean is
   begin
      return Right <= Left;
   end ">=";

   function "<" (Left : Set; Right : Set) return Boolean is
   begin
      return Element_Bags_Set_Operations."<"(Element_Bags.Bag(Left),
                                             Element_Bags.Bag(Right));
   end "<";

   function "<" (Left : Set; Right : List) return Boolean is

      Result     : Boolean;
      Right_Set  : Set;

   begin
      Add(Right, To => Right_Set);
      Result := Left < Right_Set;
      Empty(Right_Set);
      return Result;
   end "<";

   function "<" (Left : List; Right : Set) return Boolean is

      Result    : Boolean;
      Left_Set  : Set;

   begin
      Add(Left, To => Left_Set);
      Result := Left_Set < Right;
      Empty(Left_Set);
      return Result;
   end "<";

   function ">" (Left : Set; Right : Set) return Boolean is
   begin
      return Element_Bags_Set_Operations.">"(Element_Bags.Bag(Left),
                                             Element_Bags.Bag(Right));
   end ">";

   function ">" (Left : Set; Right : List) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">" (Left : List; Right : Set) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "=" (Left, Right : Set) return Boolean is
   begin
      return Element_Bags_Set_Operations."="(Element_Bags.Bag(Left),
                                             Element_Bags.Bag(Right));
   end "=";


   function Member (Element : Element_Type; Of_Set : Set) return Boolean is
   begin
      return Element_Bags.Member(Key    => Element,
                                 Of_Bag => Element_Bags.Bag(Of_Set));
   end Member;

   function Member (Element : Element_Type; Of_Set : List) return Boolean is
   begin
      for I in Of_Set'Range loop
         if Element = Of_Set(I) then
            return True;
         end if;
      end loop;
      return False;
   end Member;


   function Max (Of_Set : Set) return Element_Type is
   begin
      return Element_Bags.Max(Element_Bags.Bag(Of_Set));
   exception
      when Element_Bags.Bag_Empty =>
         raise Set_Empty;
   end Max;

   function Min (Of_Set : Set) return Element_Type is
   begin
      return Element_Bags.Min(Element_Bags.Bag(Of_Set));
   exception
      when Element_Bags.Bag_Empty =>
         raise Set_Empty;
   end Min;


   function Card (Of_Set : Set) return Natural_Count is
   begin
      return Element_Bags.Card(Element_Bags.Bag(Of_Set));
   end Card;

   function Card (Of_List : List) return Natural_Count is

      T       : Set;
      Result  : Natural_Count;

   begin
      Add(Elements => Of_List, To => T);
      Result := Card(T);
      Empty(T);
      return Result;
   end Card;


   procedure Enumeration (On_Set : in Set) is

      procedure Enumerate is new Element_Bags.Traversal(Action);

   begin
      Enumerate(Element_Bags.Bag(On_Set));
   end Enumeration;


   procedure Add (Element : in Element_Type; To : in out Set) is
   begin
      Insert(Element, Into => To);
   exception
      when Element_Bags.Duplicate_Key =>
         null;
   end Add;

   procedure Add (Elements : in Set; To : in out Set) is

      procedure Add_To_To (A : in Element_Type) is
      begin
         Add(Element => A, To => To);
      end;

      procedure Enumerate_And_Add is new Enumeration(Add_To_To);

   begin
      Enumerate_And_Add(Elements);
   end Add;

   procedure Add (Elements : in List; To : in out Set) is
   begin
      for I in Elements'Range loop
         Add(Elements(I), To);
      end loop;
   end Add;


   procedure Remove (Element : in Element_Type; From : in out Set) is
   begin
      Element_Bags.Remove(Element, From => Element_Bags.Bag(From));
   exception
      when Element_Bags.Nonexistent_Key =>
         null;
   end Remove;

   procedure Remove (Elements : in Set; From : in out Set) is

      procedure Remove_From_From (X : in Element_Type) is
      begin
         Remove(X, From);
      end;

      procedure Traverse_And_Remove is new Enumeration(Remove_From_From);

   begin
      Traverse_And_Remove(Elements);
   end Remove;

   procedure Remove (Elements : in List; From : in out Set) is
   begin
      for I in Elements'Range loop
         Remove(Elements(I), From);
      end loop;
   end Remove;


   procedure Intersect (Left : in Set; Right : in Set; To : in out Set) is

      procedure Add_To_To_If_In_Right (X : in Element_Type) is
      begin
         if Member(X, Of_Set => Right) then
            Add(Element => X, To => To);
         end if;
      end Add_To_To_If_In_Right;

      procedure Intersect_With_Right is new Enumeration(Add_To_To_If_In_Right);

   begin
      Empty(To);
      Intersect_With_Right(Left);
   end Intersect;

   procedure Intersect (Left : in Set; Right : in List; To : in out Set) is
   begin
      Empty(To);
      for I in Right'Range loop
         if Member(Right(I), Of_Set => Left) then
            Add(Element => Right(I), To => To);
         end if;
      end loop;
   end Intersect;

   procedure Intersect (Left : in List; Right : in Set; To : in out Set) is
   begin
      Intersect(Left => Right, Right => Left, To => To);
   end Intersect;


   procedure Swap (Left, Right : in out Set) is
   begin
      Element_Bags.Swap(Element_Bags.Bag(Left),
                        Element_Bags.Bag(Right));
   end Swap;


   function To_List (From : Set) return List is
   begin
      return List(Element_Bags.To_List(Element_Bags.Bag(From)));
   end To_List;

   function Empty (The_Set : Set) return Boolean is
   begin
      return Element_Bags.Empty(Element_Bags.Bag(The_Set));
   end Empty;


   procedure Assign (Object : in out Set; Value : in Set) is
   begin
      Element_Bags.Assign(Element_Bags.Bag(Object),
                          Element_Bags.Bag(Value));
   end Assign;

   procedure Assign (Object : in out Set; Value : in List) is
   begin
      Empty(Object);
      Add(Value, To => Object);
   end Assign;


   procedure Empty (The_Set : in out Set) is
   begin
      Destroy(The_Set);
   end Empty;

end General_Sets;
