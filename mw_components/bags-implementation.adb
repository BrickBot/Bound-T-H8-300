-- Filename        : bags-implementation.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : Wed Jul  1 15:34:03 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jul  1 16:45:57 1998
-- Update Count    : 9


with Bags_Opt;
with Unchecked_Deallocation;

separate (Bags)

package body Implementation is
---------------------------

   function Pointer (X : Key_Type; L : Link; First_Match : Boolean) return Link is

      Z      : Link := L;
      Result : Link := null;

   begin
      while Z /= null loop
         if X < Key_Of(Z.Val) then
            Z := Z.Left;
         elsif X = Key_Of(Z.Val) then
            Result := Z;
            exit when First_Match;
            Z := Z.Left;
         else
            Z := Z.Right;
         end if;
      end loop;
      return Result;
   end Pointer;


   procedure Deallocate is new Unchecked_Deallocation(Cell, Link);

   procedure Dispose (L : in out Link)
   is
   begin

      if Bags_Opt.Deallocate then

         Deallocate (L);

      else

         L := null;

      end if;

   end Dispose;


   procedure Balance_1 (P : in out Link; Depth_Reduced : in out Boolean) is
   begin
      case P.Balance is
         when -1 =>
            P.Balance := 0;
         when 0 =>
            P.Balance := 1;
            Depth_Reduced := False;
         when +1 =>
            -- rebalance
            declare

               P1, P2 : Link;
               B1, B2 : Equilibrium;

            begin
               P1 := P.Right;
               B1 := P1.Balance;
               if B1 >= 0 then
                  -- single RR rotation
                  P.Right := P1.Left;
                  P1.Left := P;
                  if B1 = 0 then
                     P.Balance  := 1;
                     P1.Balance := -1;
                     Depth_Reduced := False;
                  else
                     P.Balance  := 0;
                     P1.Balance := 0;
                  end if;
                  P := P1;
               else
                  -- double RL rotation
                  P2 := P1.Left;
                  B2 := P2.Balance;
                  P1.Left  := P2.Right;
                  P2.Right := P1;
                  P.Right  := P2.Left;
                  P2.Left  := P;
                  if B2 = 1 then
                     P.Balance := -1;
                  else
                     P.Balance := 0;
                  end if;
                  if B2 = -1 then
                     P1.Balance := 1;
                  else
                     P1.Balance := 0;
                  end if;
                  P := P2;
                  P2.Balance := 0;
               end if;
            end;
      end case;
   end Balance_1;


   procedure Balance_2 (P : in out Link; Depth_Reduced : in out Boolean) is
   begin
      case P.Balance is
         when +1 =>
            P.Balance := 0;
         when 0 =>
            P.Balance := -1;
            Depth_Reduced := False;
         when -1 =>
            -- rebalance
            declare

               P1, P2 : Link;
               B1, B2 : Equilibrium;

            begin
               P1 := P.Left;
               B1 := P1.Balance;
               if B1 <= 0 then
                  -- single LL rotation
                  P.Left   := P1.Right;
                  P1.Right := P;
                  if B1 = 0 then
                     P.Balance  := -1;
                     P1.Balance := 1;
                     Depth_Reduced := False;
                  else
                     P.Balance  := 0;
                     P1.Balance := 0;
                  end if;
                  P := P1;
               else
                  -- double LR rotation
                  P2 := P1.Right;
                  B2 := P2.Balance;
                  P1.Right := P2.Left;
                  P2.Left  := P1;
                  P.Left   := P2.Right;
                  P2.Right := P;
                  if B2 = -1 then
                     P.Balance := 1;
                  else
                     P.Balance := 0;
                  end if;
                  if B2 = 1 then
                     P1.Balance := -1;
                  else
                     P1.Balance := 0;
                  end if;
                  P := P2;
                  P2.Balance := 0;
               end if;
            end;
      end case;
   end Balance_2;


   procedure Action_On_Link (L : in Link) is
   begin
      Action(L.Val);
   end Action_On_Link;


   procedure Modify_On_Link (L : in Link) is

      Old_Val : constant Item_Type := L.Val;

   begin
      Modify(L.Val);
      if Key_Of(L.Val) /= Key_Of(Old_Val) then
         L.Val := Old_Val;
         raise Invalid_Key;
      end if;
   end Modify_On_Link;

end Implementation;
