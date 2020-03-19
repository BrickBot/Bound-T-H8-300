-- Storage.Bitvec_Cell_Sets (body)
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- This software is provided by the copyright holders and contributors "as is" and
-- any express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright owner or contributors be liable for
-- any direct, indirect, incidental, special, exemplary, or consequential damages
-- (including, but not limited to, procurement of substitute goods or services;
-- loss of use, data, or profits; or business interruption) however caused and
-- on any theory of liability, whether in contract, strict liability, or tort
-- (including negligence or otherwise) arising in any way out of the use of this
-- software, even if advised of the possibility of such damage.
--
-- Other modules (files) of this software composition should contain their
-- own copyright statements, which may have different copyright and usage
-- conditions. The above conditions apply to this file.
-------------------------------------------------------------------------------
--
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: storage-bitvec_cell_sets.adb,v $
-- Revision 1.2  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2009-10-07 19:26:10  niklas
-- BT-CH-0183: Cell-sets are a tagged-type class.
--


with Ada.Unchecked_Deallocation;
with Faults;
with Output;
with Storage.Cell_Store;
with Storage.Opt;


package body Storage.Bitvec_Cell_Sets is


   procedure Discard_Bitset is new Ada.Unchecked_Deallocation (
      Object => Bitset_T,
      Name   => Bitset_Ref);


   -- overriding
   procedure Adjust (Object : in out Counted_Bitset_Ref)
   is
   begin

      if Object.Ref /= null then

         Object.Ref.Ref_Count := Object.Ref.Ref_Count + 1;

      end if;

   end Adjust;


   procedure Unreference (Item : in out Bitset_Ref)
   --
   -- Removes one reference from the shared Item, which is
   -- not null on entry but may be null on return, if the
   -- Item was the last reference to this shared Bitset.
   --
   is
   begin

       Item.Ref_Count := Item.Ref_Count - 1;

       if Item.Ref_Count > 0 or not Deallocate then
          -- There are other users of this Bitset, or we
          -- are asked not to deallocate useless storage.

          Item := null;

       else
          -- This Item was the last reference to Item.all, and
          -- we are asked to deallocate useless storage.

          Discard_Bitset (Item);

       end if;

   end Unreference;


   -- overriding
   procedure Finalize (Object : in out Counted_Bitset_Ref)
   is
   begin

      if Object.Ref /= null then

         Unreference (Object.Ref);

     end if;

   end Finalize;


   procedure Ensure_Ownership (Object : in out Set_T)
   --
   -- Ensures that this Object is the sole user (and thus the owner)
   -- of its Member set.
   --
   is
      Ref : Bitset_Ref renames Object.Set.Ref;
   begin

      if Ref /= null
      and then Ref.Ref_Count > 1
      then
          -- This Object shares its Member.Ref with some other sets, so
          -- it must now be given its own copy of the Member.Ref.

          Ref.Ref_Count := Ref.Ref_Count - 1;
          -- Since Ref_Count was > 1 it remains > 0.

          Ref := new Bitset_T'(Ref.all);

          Ref.Ref_Count := 1;

       end if;

   end Ensure_Ownership;


   -- overriding
   function Empty return Set_T
   is
   begin

      return (Root_Cell_Set_T with Set => (
         Ada.Finalization.Controlled with Ref => null));

   end Empty;


   -- overriding
   procedure Erase (Set : in out Set_T)
   is
   begin

      Finalize (Set.Set);

   end Erase;


   function Is_Empty (Item : Bitvec_T) return Boolean
   --
   -- Whether the Item is empty, that is Item contains only False elements.
   --
   is
   begin

      for I in Item'Range loop

         if Item(I) then

            return False;

         end if;

      end loop;

      return True;

   end Is_Empty;


   -- overriding
   function Is_Empty (Item : Set_T) return Boolean
   is
   begin

      return  Item.Set.Ref = null
      or else Is_Empty (Item.Set.Ref.Bitvec);

   end Is_Empty;


   -- overriding
   function Is_Member (
      Cell   : Cell_T;
      Of_Set : Set_T)
   return Boolean
   is
   begin

      if Of_Set.Set.Ref = null then

         return False;

      else

         declare
            I : constant Cell_Index_T := Index (Cell);
         begin

            return   I in Of_Set.Set.Ref.Bitvec'Range
            and then Of_Set.Set.Ref.Bitvec(I);

         end;

      end if;

   end Is_Member;


   -- overriding
   function Card (Set : Set_T) return Natural
   is

      Ref : Bitset_Ref renames Set.Set.Ref;

      Tally : Natural := 0;
      -- The result.

   begin

      if Ref /= null then

         for M in Ref.Bitvec'Range loop

            if Ref.Bitvec(M) then

               Tally := Tally + 1;

            end if;

         end loop;

      end if;

      return Tally;

   end Card;


   -- overriding
   function To_Set (List : Cell_List_T) return Set_T
   is

      Set : Set_T;
      -- The result, initially empty.

   begin

      Add (Cells => List, To => Set);

      return Set;

   end To_Set;


   -- overriding
   function To_List (Set : Set_T) return Cell_List_T
   is

      Ref : Bitset_Ref renames Set.Set.Ref;

      List : Cell_List_T (1 .. Number_Of_Cells);
      Last : Natural := 0;

   begin

      if Ref /= null then

         for M in Ref.Bitvec'Range loop

            if Ref.Bitvec(M) then

               Last := Last + 1;
               List(Last) := Cell_Store.Cell_At (M);

            end if;

         end loop;

      end if;

      return List(1 .. Last);

   end To_List;


   -- overriding
   procedure Add (
      Cell : in     Cell_T;
      To   : in out Set_T)
   is
   begin

      if To.Set.Ref = null then

         To.Set.Ref := new Bitset_T'(
            Max_Index => Cell_Index_T (Number_Of_Cells),
            Ref_Count => 1,
            Bitvec    => (others => False));

      else

         Ensure_Ownership (To);

      end if;

      To.Set.Ref.Bitvec(Index (Cell)) := True;

   end Add;


   -- overriding
   procedure Add (
      Cells : in     Set_T;
      To    : in out Set_T)
   is

      Sref : Bitset_Ref renames Cells.Set.Ref;
      Tref : Bitset_Ref renames To.Set.Ref;
      -- The source and target bitsets.

   begin

      if Sref = null
      or Sref = Tref
      then
         -- Nothing to add, or adding a set to the same set.
         -- No effect in either case.

         null;

      elsif Tref = null then
         -- Adding Cells To a null set. We can do this
         -- quickly by copying the reference.

         Tref := Sref;

         Sref.Ref_Count := Sref.Ref_Count + 1;

      else
         -- Adding some cells to a non-null set. Some work.

         Ensure_Ownership (To);
         -- This may change Tref, but not to null or Sref.

         declare

            SV : Bitvec_T renames Sref.Bitvec;
            TV : Bitvec_T renames Tref.Bitvec;

         begin

            if Tref.Max_Index = Sref.Max_Index then
                -- We can directly "or" the bitvectors.

               TV := TV or SV;

            elsif Tref.Max_Index < Sref.Max_Index then
               -- We can "or" the TV part.

               TV := TV or SV(TV'Range);

               -- Check that no cells are left out:

               for S in TV'Last + 1 .. SV'Last loop

                  if SV(S) then
                     -- Oh oh, this cell should be added to TV
                     -- but is beyond the index range of TV.

                     raise Constraint_Error;

                  end if;

               end loop;

            else
               -- Tref.Max_Index > Sref.Max_Index
               -- Only SV'Range is affected in TV.

               TV(SV'Range) := TV(SV'Range) or SV;

            end if;

         end;

      end if;

   end Add;


   -- overriding
   procedure Remove (
      Cell : in     Cell_T;
      From : in out Set_T)
   is
   begin

      if From.Set.Ref /= null then

         Ensure_Ownership (From);

         From.Set.Ref.Bitvec(Index (Cell)) := False;

      end if;

   end Remove;


   -- overriding
   procedure Move_Cells (
      From : in out Set_T;
      To   : in out Set_T)
   is

      From_Set_Ref : Bitset_Ref := From.Set.Ref;

   begin

      if From_Set_Ref = null then
         -- Nothing to move; no effect.

         null;

      elsif To.Set.Ref = null then
         -- Simple case: move the reference.

         To.Set.Ref := From_Set_Ref;
                       From.Set.Ref := null;

      elsif From_Set_Ref = To.Set.Ref then
         -- The From and To set have the same elements, so
         -- no effect on To. However, From may become empty
         -- if it is not an alias of To.

         From.Set.Ref := null;

         if To.Set.Ref = null then
            -- Aha, From and To are the same object. Thus
            -- this procedure shall have no effect on either.

            From.Set.Ref := From_Set_Ref;
            -- This also restores To.Set.Ref.

         else
            -- From and To are different objects. Thus
            -- this procedure shall leave From empty, so
            -- we remove the reference:

            Unreference (From_Set_Ref);

         end if;

      else
         -- From is not trivially null and is not the same
         -- object as To (which is not trivially null either,
         -- but that is not relevant).

         Add (Cells => From, To => To);

         Erase (From);

      end if;

   end Move_Cells;


   -- overriding
   function Copy (Item : Cell_Set_T) return Set_T
   is
   begin

      if Item in Set_T then
         -- Just a copy, no type conversion.

         return Set_T (Item);

      else
         -- Copy some other kind of cell-set.

         return To_Set (To_List (Item));

      end if;

   end Copy;


   function "=" (Left, Right : Set_T) return Boolean
   is

      Lref : Bitset_Ref renames Left.Set.Ref;
      Rref : Bitset_Ref renames Right.Set.Ref;

   begin

      if Lref = Rref then
         -- Left and Right refer to the same cell-set object
         -- so they are equal, of course.

         return True;

      elsif Lref = null then
         -- Left is empty, so we check if Right is empty, too.

         return Is_Empty (Right);

      elsif Rref = null then
         -- Ditto but vice versa.

         return Is_Empty (Left);

      elsif Lref.Max_Index = Rref.Max_Index then
         -- We can directly compare the bit-sets.

         return Lref.Bitvec = Rref.Bitvec;

      elsif Lref.Max_Index < Rref.Max_Index then
         -- For equality, Left and Right must be equal on Left'Range
         -- and the rest of Right must be empty.

         declare
             Lvec : Bitvec_T renames Lref.Bitvec;
             Rvec : Bitvec_T renames Rref.Bitvec;
         begin

         return   Lvec = Rvec(Lvec'Range)
         and then Is_Empty (Rvec(Lvec'Last + 1 .. Rvec'Last));

         end;

      else
         -- Lref.Max_Index > Rref.Max_Index. Vice versa.

         declare
             Lvec : Bitvec_T renames Lref.Bitvec;
             Rvec : Bitvec_T renames Rref.Bitvec;
         begin

         return   Lvec(Rvec'Range) = Rvec
         and then Is_Empty (Lvec(Rvec'Last + 1 .. Lvec'Last));

         end;

      end if;

   end "=";


   function Union (Left, Right : Set_T) return Cell_Set_T
   is

      Lref : Bitset_Ref renames Left.Set.Ref;
      Rref : Bitset_Ref renames Right.Set.Ref;

   begin

      if Lref = null
      or Lref = Rref
      then
         -- The Left set is empty, so the union is the Right set,
         -- or the two sets are the same, and so is the union.

         return Right;

      elsif Rref = null then
         -- Vice versa.

         return Left;

      else
         -- Neither set is trivially empty, nor are they the same set.
         -- Drat, we must do some work.

         declare

            Uni : Set_T := (Root_Cell_Set_T with
               Set => (Ada.Finalization.Controlled with
                  Ref => new Bitset_T'(
                     Max_Index => Cell_Index_T'Max (
                        Lref.Max_Index,
                        Rref.Max_Index),
                     Ref_Count => 1,
                     Bitvec    => <>)));
            -- The union to be.

            LV : Bitvec_T renames Lref.Bitvec;
            RV : Bitvec_T renames Rref.Bitvec;
            UV : Bitvec_T renames Uni.Set.Ref.Bitvec;

         begin

            if LV'Last = RV'Last then
                -- We can directly "or" the bitvectors.

               UV := LV or RV;

            elsif LV'Last < RV'Last then
               -- The union will be "Left or Right" on LV'Range, and
               -- for the rest it will be a copy of RV.

               UV(LV'Range) := LV or RV(LV'Range);

               UV(LV'Last + 1 .. RV'Last) := RV(LV'Last + 1 .. RV'Last);

            else
               -- Vice versa.

               UV(RV'Range) := LV(RV'Range) or RV;

               UV(RV'Last + 1 .. UV'Last) := LV(RV'Last + 1 .. LV'Last);

            end if;

            return Uni;

         end;

      end if;

   end Union;


   function "-"  (Left, Right : Set_T) return Cell_Set_T
   is

      Lref : Bitset_Ref renames Left.Set.Ref;
      Rref : Bitset_Ref renames Right.Set.Ref;

   begin

      if Lref = null
      or Lref = Rref
      then
         -- The Left set is empty, or equals the Right set.

         return Set_T'(Empty);

      elsif Rref = null then
         -- The Right set (to be subtracted) is empty.

         return Left;

      else
         -- Neither set is trivially empty, nor are they the same set.
         -- Drat, we must do some work.

         declare

            Diff : Set_T := (Root_Cell_Set_T with
               Set => (Ada.Finalization.Controlled with
                  Ref => new Bitset_T'(
                     Max_Index => Lref.Max_Index,
                     Ref_Count => 1,
                     Bitvec    => <>)));
            -- The difference to be.

            LV : Bitvec_T renames Lref.Bitvec;
            RV : Bitvec_T renames Rref.Bitvec;
            DV : Bitvec_T renames Diff.Set.Ref.Bitvec;

         begin

            if LV'Last = RV'Last then
               -- We can directly compute the whole difference vector.

               DV := LV and not RV;

            elsif LV'Last < RV'Last then
               -- We can ignore the rest of Right.

               DV := LV and not RV(LV'Range);

            else
               -- LV'Last > RV'Last.
               -- The rest of Left is copied to the result without change.

               DV(RV'Range) := LV(RV'Range) and not RV;

               DV(RV'Last + 1 .. DV'Last) := LV(RV'Last + 1 .. LV'Last);

            end if;

            return Diff;

         end;

      end if;

   end "-";


   function Intersection (Left, Right : Set_T) return Cell_Set_T
   --
   -- The intersection of the given cell-sets.
   --
   is

      Lref : Bitset_Ref renames Left.Set.Ref;
      Rref : Bitset_Ref renames Right.Set.Ref;

   begin

      if Lref = null
      or Lref = Rref
      then
         -- The Left set is empty, or equals the Right set,
         -- and so the intersection equals the Left set.

         return Left;

      elsif Rref = null then
         -- The Right set is empty, and so is the intersection.

         return Set_T'(Empty);

      else
         -- Neither set is trivially empty, nor are they the same set.
         -- Drat, we must do some work.

         declare

            Int : Set_T := (Root_Cell_Set_T with
               Set => (Ada.Finalization.Controlled with
                  Ref => new Bitset_T'(
                     Max_Index => Cell_Index_T'Min (
                        Lref.Max_Index,
                        Rref.Max_Index),
                     Ref_Count => 1,
                     Bitvec    => <>)));
            -- The intersection, to be.

            LV : Bitvec_T renames Lref.Bitvec;
            RV : Bitvec_T renames Rref.Bitvec;
            IV : Bitvec_T renames Int.Set.Ref.Bitvec;

         begin

            if LV'Last = RV'Last then
               -- No need to slice.

               IV := LV and RV;

            else
               -- Either LV or RV must be sliced, depending on
               -- which is longer. We slice both.

               IV := LV(IV'Range) and RV(IV'Range);

            end if;

            return Int;

         end;

      end if;

   end Intersection;


   procedure Print_Debug_Pool
   is
   begin

      --:dbpool GNAT.Debug_Pools.Print_Info_Stdout (Bitset_Pool);

      null;

   end Print_Debug_Pool;


end Storage.Bitvec_Cell_Sets;
