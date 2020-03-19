-- Options.Traversal (body)
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
-- $Date: 2015/10/24 20:05:50 $
--
-- $Log: options-traversal.adb,v $
-- Revision 1.2  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Ada.Text_IO;


package body Options.Traversal is


   function Top_Group (Among : Group_Set_T) return Group_Index_T
   --
   -- The group with the highest (numerically smallest) priority,
   -- from Among a given set of groups.
   --
   -- Propagates Constraint_Error if the given group set is empty.
   --
   is

      Cand_Known : Boolean := False;
      -- We have a candidate for the top position.

      Cand_Index : Group_Index_T;
      -- The (index of the) candidate group.

      Cand_Prio : Group_Priority_T := Group_Priority_T'Last;
      -- The priority of the candidate group.
      -- Initialization is to disable GNAT warnings.

   begin

      for A in Among'Range loop

         if Among(A) then

            if (not Cand_Known)
            or else Priority_Of (A) < Cand_Prio
            then
               -- The first or best candidate we have found.

               Cand_Known := True;
               Cand_Index := A;
               Cand_Prio  := Priority_Of (A);

            end if;

         end if;

      end loop;

      if not Cand_Known then
         -- The group set is empty.

         raise Constraint_Error;

      end if;

      return Cand_Index;

   end Top_Group;


   type Group_List_T is array (Positive range <>) of Group_Index_T;
   --
   -- A list of (indices of) groups.


   function Sorted (Groups : Group_Set_T) return Group_List_T
   --
   -- The Groups in high-to-low priority order.
   --
   is

      List : Group_List_T (1 .. Groups'Length);
      Last : Natural := 0;
      -- The result will be List(1 .. Last).

      Rest : Group_Set_T := Groups;
      -- The rest of the Groups, not yet in List.

      Top : Group_Index_T;
      -- The highest-priority of the Rest groups.

   begin

      while Rest /= Null_Group_Set loop

         Top := Top_Group (Rest);

         Last       := Last + 1;
         List(Last) := Top;

         Rest(Top) := False;

      end loop;

      return List(1 .. Last);

   end Sorted;


   procedure Enter_All (
      Groups  : in     Group_List_T;
      Visitor : in out Visitor_T'Class)
   --
   -- Enters all the Groups in the given order.
   --
   is
   begin

      for G in Groups'Range loop

         Enter_Group (Visitor, Group_Table(Groups(G)));

      end loop;

   end Enter_All;


   procedure Leave_All (
      Groups  : in     Group_List_T;
      Visitor : in out Visitor_T'Class)
   --
   -- Leaves all the Groups in the given order, reversed.
   --
   is
   begin

      for G in reverse Groups'Range loop

         Leave_Group (Visitor, Group_Table(Groups(G)));

      end loop;

   end Leave_All;


   function Choose (
      From   : Option_List_T;
      Group  : Group_Index_T;
      Member : Boolean)
   return Option_List_T
   --
   -- Those options From the given list, that are/are not Members
   -- of the given Group.
   --
   is

      Result : Option_List_T (1 .. From'Length);
      Last   : Natural := 0;
      -- The result is Result(1 .. Last).

   begin

      for F in From'Range loop

         if From(F).Groups(Group) = Member then

            Last := Last + 1;
            Result(Last) := From(F);

         end if;

      end loop;

      return Result(1 .. Last);

   end Choose;


   procedure Traverse (
      List     : in     Option_List_T;
      Within   : in     Group_Set_T := Null_Group_Set;
      Synonyms : in     Boolean;
      Visitor  : in out Visitor_T'Class)
   is

      -- Principles of operation:
      --
      -- First we find the "common" set of groups to which all
      -- these options belong (intersection of group sets) and
      -- the "whole" set of groups to which any options below (union
      -- of group sets).
      --
      -- The "differential" groups are those groups to which some
      -- options belong, but to which not all options belong. This
      -- is the set difference between the whole and common sets
      -- of groups.
      --
      -- Now we present (show) all options that belong exactly
      -- to the common set of groups, and only to those groups,
      -- in the same order as in the List.
      --
      -- If there are any List options left, we divide them into
      -- two sets based on the "top differential group" which is
      -- the differential group with the highest priority, and
      -- recursively traverse in the same manner first the options
      -- that belong to the top group, and then those that do not
      -- belong to the top group.

      Common : Group_Set_T := (others => True);
      -- The groups to which all List options belong

      Whole : Group_Set_T := (others => False);
      -- The groups to which some (any) List option belongs.

      Differ : Group_Set_T;
      -- The groups to which some, but not all, List options belong.

      Current : Group_Set_T;
      -- The currently "entered" set of groups.

      Rest : Option_List_T (1 .. List'Length);
      Last : Natural := 0;
      -- The rest of the Listed options, not yet traversed,
      -- are Rest(1 .. Last).

   begin

      --Ada.Text_IO.Put_Line (
      --     "Traversing"
      --   & Natural'Image (List'Length)
      --   & " options, Within groups "
      --   & Image (Within));

      -- Find the common and whole sets of groups:

      for L in List'Range loop

         if Synonyms or not Is_Synonym (List(L)) then

            Common := Common and List(L).Groups;
            Whole  := Whole  or  List(L).Groups;

         end if;

      end loop;

      Differ := Whole and not Common;

      --Ada.Text_IO.Put_Line ("Common groups: " & Image (Common));
      --Ada.Text_IO.Put_Line ("Whole  groups: " & Image (Whole ));
      --Ada.Text_IO.Put_Line ("Differ groups: " & Image (Differ));

      if  Common  = Null_Group_Set
      and Differ /= Null_Group_Set
      then
         -- There are no common groups, but there are
         -- some differentiating groups.

          Last := List'Length;
          Rest(1 .. Last) := List;

          Current := Within;

      else
         -- There are some common groups, or there are no
         -- differentiating groups (a special case is that
         -- these options belong to no groups at all).

         -- Visit the options that belong only to the common groups:

         Enter_All (Sorted (Common and not Within), Visitor);

         Current := Common or Within;

         for L in List'Range loop

            if Is_Synonym (List(L)) and not Synonyms then
               -- A synonym to be skipped.

               null;

            elsif List(L).Groups = Common then
               -- This option belongs only to the Common groups.
               -- Visit it now.

               Visit (Visitor, List(L));

            else
               -- This option belongs also to some other groups.
               -- Visit it later.

               Last := Last + 1;

               Rest(Last) := List(L);

            end if;

         end loop;

      end if;

      if Last > 0 then
         -- Some of the List options are not yet Visited.
         -- This implies that there are some Differ groups.

         declare

            Top : constant Group_Index_T := Top_Group (Differ);

            Outside : constant Option_List_T := Choose (
               From   => Rest(1 .. Last),
               Group  => Top,
               Member => False);
            -- Those options from Rest that do not belong to the Top group.
            -- This set can be empty.

            Inside : constant Option_List_T := Choose (
               From   => Rest(1 .. Last),
               Group  => Top,
               Member => True);
            -- Those options from Rest that belong to the Top group.
            -- This set is non-empty; if it were empty, some options
            -- already visited would have to belong to the Top group,
            -- which means that Top would be one of the Common groups,
            -- which is impossible because Top is a Differ group.

         begin

            --Ada.Text_IO.Put_Line (
            --     "Top Differ group is "
            --   & String_Pool.To_String (Group_Table(Top).Name));

            Enter_Group (
               Group   => Group_Table(Top),
               Visitor => Visitor);

            Current(Top) := True;

            Traverse (
               List     => Inside,
               Within   => Current,
               Synonyms => Synonyms,
               Visitor  => Visitor);

            Leave_Group (
               Group   => Group_Table(Top),
               Visitor => Visitor);

            Current(Top) := False;

            if Outside'Length > 0 then

               Traverse (
                  List     => Outside,
                  Within   => Current,
                  Synonyms => Synonyms,
                  Visitor  => Visitor);

            end if;

         end;

      end if;

      if Common /= Null_Group_Set
      or Differ  = Null_Group_Set
      then
         -- We entered the Common groups, above, so we
         -- should also leave them.

         Leave_All (Sorted (Current and not Within), Visitor);

      end if;

   end Traverse;


end Options.Traversal;
