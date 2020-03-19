-- GENERIC QUICK SORT PROCEDURE
   ----------------------------

-- Revision :  3-JUN-1988 by Mats Weber, use a median of three method to select the pivot.

-- Creation : 15-JUN-1986 by Mats Weber


with Exchange;

procedure Quick_Sort (Items : in out Vector) is
--------------------------------------------

   procedure Sort (Left, Right : in Index) is

      subtype Curr_Range is Index range Left..Right;

      Middle : constant Curr_Range := Index'Val((Index'Pos(Left) + Index'Pos(Right)) / 2);
      Pivot  : Item;
      I      : Curr_Range := Index'Succ(Left);
      J      : Curr_Range := Index'Pred(Right);

      procedure Swap is new Exchange(Item);

   begin
      if Items(Middle) < Items(Left) then
         Swap(Items(Middle), Items(Left));
      end if;
      if Items(Right) < Items(Left) then
         Swap(Items(Right), Items(Left));
      end if;
      if Items(Right) < Items(Middle) then
         Swap(Items(Right), Items(Middle));
      end if;
      Pivot := Items(Middle);
      Swap(Items(Middle), Items(J));
      if J /= Left then
         J := Index'Pred(J);
      end if;
      loop
         while Items(I) < Pivot loop
            I := Index'Succ(I);
         end loop;
         while Pivot < Items(J) loop
            J := Index'Pred(J);
         end loop;
         exit when I > J;
         if I = Right or J = Left then
            return;
         else
            Swap(Items(I), Items(J));
            I := Index'Succ(I);
            J := Index'Pred(J);
            exit when I > J;
         end if;
      end loop;
      if Left < J then
         Sort(Left, J);
      end if;
      if I < Right then
         Sort(I, Right);
      end if;
   end Sort;

begin
   if Items'First < Items'Last then
      Sort(Items'First, Items'Last);
   end if;
end Quick_Sort;
