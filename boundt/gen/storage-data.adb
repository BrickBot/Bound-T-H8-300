-- Storage.Data (body)
--
-- Author: Niklas Holsti
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: storage-data.adb,v $
-- Revision 1.4  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.3  2009-11-27 11:28:08  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.2  2007/08/25 19:43:37  niklas
-- BT-CH-0075: Storage.Data corrected to store Step_Data_Ref.
--
-- Revision 1.1  2007/08/20 12:24:32  niklas
-- First version.
--


with Arithmetic.Logic;
with Hash_G.Changes_G;            -- LGL components.
with Output;
with Storage.Data.Opt;


package body Storage.Data is


   --
   ---   State_T
   --


   Known : constant Arithmetic.Evaluation.Level_T :=
      Arithmetic.Evaluation.Known;


   function Image (Item : State_T) return String
   is
   begin

      return
           '['
         & Hash_Value_T'Image (Item.Hash)
         & " ] "
         & Image (Item.Values(1 .. Item.Tally));

   end Image;


   procedure Erase (State : in out State_T)
   is
   begin

      State.Tally := 0;
      State.Hash  := 0;

   end Erase;


   function Copy (
      State  : in State_T;
      Growth : in Natural)
   return State_T
   is

      Tally : constant Natural := State.Tally;

      Dup : State_T (Capacity => State.Capacity + Growth);

   begin

      Dup.Tally := Tally;

      Dup.Hash  := State.Hash;

      Dup.Values(1 .. Tally) := State.Values(1 .. Tally);

      return Dup;

   end Copy;


   procedure Find (
      Cell   : in     Cell_T;
      Within : in     Cell_Value_List_T;
      Slot   :    out Positive)
   --
   -- Looks up the Cell, Within the cell-value list, and returns
   -- the smallest Slot where Index (Within(Slot).Cell) >= Index (Cell).
   -- If there is no such element Within the list, Slot is returned
   -- as Within'First + Within'Length, which is always > Within'Last.
   --
   -- Precondition: The list is ordered strictly ascending by Index.
   --
   is

      X : constant Cell_Index_T := Index (Cell);
      -- The index we are looking for.

      First : Positive := Within'First;
      Last  : Natural  := Within'Last;
      -- The interval being searched.

      Mid : Positive;
      -- The middle of the interval.

      Mid_Index : Cell_Index_T;
      -- The index of the Mid cell.

   begin

      if Within'Length = 0 or else X <= Index(Within(First).Cell) then
         -- The Cell comes at or before the First one.

         Slot := First;

      elsif X > Index (Within(Last).Cell) then
         -- The cell comes after all the elements in the list.

         Slot := Last + 1;

      else
         -- The cell comes after First and before or at Last.
         -- First < Last.

         -- Binary search to set First to the slot.

         loop
            -- First < Last.
            -- Index(First) < X <= Index(Last).

            Mid := First + (Last - First) / 2;
            -- First <= Mid < Last.

            Mid_Index := Index (Within(Mid).Cell);

            if Mid_Index = X then
               -- We found the cell.

               First := Mid;

               exit;

            elsif Mid_Index < X then

               First := Mid + 1;
               -- First is increased.

               exit when Index (Within(First).Cell) >= X;
               -- On exit Index(First - 1) < X, so First is the Slot;
               -- else Index(First) < X so First < Last.

            else
               -- X < Mid_Index.
               -- Therefore First < Mid.

               Last := Mid;
               -- Last is decreased but still First < Last.

            end if;

         end loop;

         Slot := First;

      end if;

   end Find;


   procedure Hash (
      Binding : in    Cell_Value_T;
      Into    : in out Hash_Value_T)
   --
   -- Applies the contribution of a given cell-value Binding
   -- Into the hash of a State/Hold.
   --
   is
     use type Arithmetic.Word_T;
   begin

      Into := Into
          xor Hash_Value_T (Index (Binding.Cell) mod Hash_Mod)
          xor Hash_Value_T (       Binding.Value mod Hash_Mod);

   end Hash;


   procedure Bind (
      Cell  : in     Cell_T;
      Value : in     Arithmetic.Word_T;
      Under : in out State_T)
   is

      Values : Cell_Value_List_T renames Under.Values;
      Tally  : Natural           renames Under.Tally;

      Slot : Positive;
      -- The binding goes into Values(Slot).

   begin

      Find (
         Cell   => Cell,
         Within => Values(1 .. Tally),
         Slot   => Slot);

      if       Slot <= Tally
      and then Values(Slot).Cell = Cell
      then
         -- Update the existing binding:

         Hash (Binding => Values(Slot), Into => Under.Hash);

         Values(Slot).Value := Value;

      else
         -- A new cell to be bound.

         if Tally >= Under.Capacity then

            Output.Fault (
               Location => "Storage.Data.Bind",
               Text     => "Hold capacity exceeded.");

            return;

         end if;

         Tally := Tally + 1;

         -- Shift the later cells along:

         for T in reverse Slot + 1 .. Tally loop

            Values(T) := Values(T - 1);

         end loop;

         -- Insert the new binding:

         Values(Slot) := (Cell, Value);

      end if;

      Hash (Binding => Values(Slot), Into => Under.Hash);

   end Bind;


   procedure Unbind (
      Cell  : in     Cell_T;
      Under : in out State_T)
   is

      Values : Cell_Value_List_T renames Under.Values;
      Tally  : Natural           renames Under.Tally;

      Slot : Natural;
      -- The slot for the binding.

   begin

      Find (
         Cell   => Cell,
         Within => Values(1 .. Tally),
         Slot   => Slot);

      if       Slot <= Tally
      and then Values(Slot).Cell = Cell
      then
         -- The cell is bound, so we must remove this binding.

         Hash (Binding => Values(Slot), Into => Under.Hash);

         for T in Slot .. Tally - 1 loop

            Values(T) := Values(T + 1);

         end loop;

         Tally := Tally - 1;

      end if;

   end Unbind;


   function Value_Of (
      Cell  : Cell_T;
      Under : State_T)
   return Eval_Value_T
   is

      Values : Cell_Value_List_T renames Under.Values;
      Tally  : Natural           renames Under.Tally;

      Slot : Positive;
      -- The slot for the binding.

   begin

      Find (
         Cell   => Cell,
         Within => Values(1 .. Tally),
         Slot   => Slot);

      if       Slot <= Tally
      and then Values(Slot).Cell = Cell
      then
         -- The cell is bound.

         return (
            Level  => Known,
            Value  => Values(Slot).Value,
            Width  => Cell.Width,
            Signed => False);

      else
         -- The cell is not bound.

         return Unbound;

      end if;

   end Value_Of;


   function Hash_Of (Item : State_T) return Natural
   is
   begin

      return Natural (Item.Hash);
      --
      -- Assumes that any Hash_Value_T fits in Natural.

   end Hash_Of;


   --
   ---   Space_T
   --


   package Hashed_States_Changes is new Hashed_States.Changes_G;


   procedure Prepare (Space : in out Space_T)
   is
   begin

      Space := Create (Minimum_Size => 217);

   end Prepare;


   procedure Store (
      From  : in     Step_Data_Ref;
      State : in     State_T;
      Into  : in     Space_Ref;
      Ref   :    out Step_Data_Ref)
   is

      Old_Size, New_Size : Positive;
      -- The old and new sizes of the hash table, when we
      -- had to expand it.

      Tally : Natural;
      -- The number of bindings in the State.

   begin

      Ref := Retrieve (
         Hash_Table => Into.all,
         Key        => State);

      -- The State is already known, Ref is returned.

      if Opt.Trace_Space_Ops then

         Output.Trace (
             "Retrieved data state"
            & Output.Field_Separator
            & Flow.Image (Flow.Step_Data_Ref (Ref)));

      end if;

   exception

   when Hashed_States.Key_Not_Found_Error =>
      -- This State is a new one, for this space.

      if Is_Full (Into.all) then

         Old_Size := Size (Into.all);
         New_Size := Old_Size + 511;

         Output.Note (Text =>
            "Storage.Data.State hash-table was grown from"
            & Positive'Image (Old_Size)
            & " to (at least)"
            & Positive'Image (New_Size)
            & " elements.");

         Hashed_States_Changes.Resize (
            Hash_Table   => Hashed_States.Hash_Table_Type (Into.all),
            Minimum_Size => New_Size);

      end if;

      -- Store the new State, discarding any unused capacity:

      Tally := State.Tally;

      Ref := new Step_Data_T'Class'(From.all);

      Ref.Space := Into;
      Ref.State := new State_T'(
         Capacity => Tally,
         Tally    => Tally,
         Hash     => State.Hash,
         Values   => State.Values(1 .. Tally));

      if Opt.Trace_Space_Ops then

         Output.Trace (
              "Created data state"
            & Flow.Image (Flow.Step_Data_Ref (Ref)));

      end if;

      Insert (
         Hash_Table => Into.all,
         Key        => State,
         Data       => Ref);

   end Store;


   procedure Discard (Space : in out Space_T)
   is
   begin

      Clear (Space);

   end Discard;


   --
   ---   Domain_T, an evaluation domain based on a State.
   --


   -- overriding
   function Basis (Item : Domain_T)
   return Storage.Cell_List_T
   is
   begin

      return Cells_Of (Item.State.Values(1 .. Item.State.Tally));

   end Basis;


   -- overriding
   function Value_Of (Cell : Cell_T; Within : Domain_T)
   return Eval_Value_T
   is
   begin

      return Value_Of (Cell, Within.State.all);

   end Value_Of;


   --
   ---   Step_Data_T
   --


   -- overriding
   function Domain (Data : access Step_Data_T)
   return Arithmetic.Evaluation.Domain_T'Class
   is
   begin

      return Domain_T'(State => Data.State);

   end Domain;


   function Same_Or_New (
      From : access Step_Data_T;
      Post :        State_T)
   return Flow.Step_Data_Ref
   --
   -- The data-state that originates From a given data-state and has
   -- a transformed or constrained Post state, which may be the same
   -- as the original (From.Domain.State) or may be new. If new, the
   -- new state is stored in From.Space and returned.
   --
   is

      Result : Step_Data_Ref;
      -- The transformed Step_Data, in Ref.

   begin

      if Post = From.State.all then
         -- Status quo as far as we are concerned.

         Result := Step_Data_Ref (From);

      else
         -- Mutatis...

         Store (
            From  => Step_Data_Ref (From),
            State => Post,
            Into  => From.Space,
            Ref   => Result);

      end if;

      return Flow.Step_Data_Ref (Result);

   end Same_Or_New;


   -- overriding
   function Transformed_Data (
      From  : access Step_Data_T;
      After :        Arithmetic.Effect_T)
   return Flow.Step_Data_Ref
   is
      use Arithmetic;

      Post : State_T := Copy (
         State  => From.State.all,
         Growth => After'Length);
      -- The state to be transformed, but initially not so.
      -- Any assignment in After can add a cell binding, in
      -- the worst case.

      Asg : Assignment_T;
      -- A part of After.

   begin

      -- First we unbind all cells that are assigned something other
      -- than a constant by some value-defining assignment.
      -- We can ignore Fracture assignments because the corresponding
      -- value-defining assignment will override the Fracture.

      for A in After'Range loop

         Asg := After(A);

         if  Asg.Target.Kind = Cell
         and (Asg.Kind = Conditional
              or else (Asg.Kind = Regular
                       and then Asg.Value.Kind /= Const))
         then
            -- An assignment of a non-constant to a cell.

            Unbind (Cell => Asg.Target.Cell, Under => Post);

         end if;

      end loop;

      -- Then we bind all cells that are assigned constant values.

      for A in After'Range loop

         Asg := After(A);

         if (Asg.Target.Kind = Cell and Asg.Kind = Regular)
         and then Asg.Value.Kind = Const
         then

            Bind (
               Cell  => Asg.Target.Cell,
               Value => Asg.Value.Value,
               Under => Post);

         end if;

      end loop;

      -- Any changes?

      return Same_Or_New (From, Post);

   end Transformed_Data;


   -- overriding
   function Constrained_Data (
      From : access Step_Data_T;
      By   :        Arithmetic.Condition_T)
   return Flow.Step_Data_Ref
   is

      Implied : constant Storage.Cell_Value_List_T :=
         Arithmetic.Logic.Cell_Values_Implied (By);
      -- The constant cell values implied by the condition.

      Post : State_T := Copy (
         State  => From.State.all,
         Growth => Implied'Length);
      -- The state to be constrained, but initially not so.
      -- Each implied cell value can add a binding.

   begin

      -- Apply the Implied values to the state:

      for I in Implied'Range loop

         Bind (
            Cell  => Implied(I).Cell,
            Value => Implied(I).Value,
            Under => Post);

      end loop;

      -- Any changes?

      return Same_Or_New (From, Post);

   end Constrained_Data;


   -- overriding
   function Image (Item : Step_Data_T) return String
   is
   begin

      return Image (Item.State.all);

   end Image;


   -- not overriding
   procedure Set_Initial_State (
      Values : in Cell_Value_List_T;
      Data   : in Step_Data_Ref)
   is
   begin

      Data.Space := new Space_T;
      Data.State := new State_T (Capacity => Values'Length);

      for V in Values'Range loop

         Bind (
            Cell  => Values(V).Cell,
            Value => Values(V).Value,
            Under => Data.State.all);

      end loop;

      Prepare (Data.Space.all);

      Insert (
         Hash_Table => Data.Space.all,
         Key        => Data.State.all,
         Data       => Data);

   end Set_Initial_State;


   -- not overriding
   function Number_Of_States (Data : Step_Data_T)
   return Natural
   is
   begin

      return Element_Count (Data.Space.all);

   end Number_Of_States;


   ---   Space_T


   function Same_State (Left, Right : State_T) return Boolean
   is
   begin

      return (Left.Hash  = Right.Hash
         and  Left.Tally = Right.Tally)
         and then
              Left.Values(1 .. Left.Tally)
            = Right.Values(1 .. Left.Tally);

   end Same_State;


end Storage.Data;
