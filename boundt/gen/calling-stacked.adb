-- Calling.Stacked (body)
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: calling-stacked.adb,v $
-- Revision 1.3  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.2  2007-12-17 13:54:35  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.1  2005/02/23 09:05:17  niklas
-- BT-CH-0005.
--


with Ada.Strings.Unbounded;
with Flow;
with Output;


package body Calling.Stacked is


   function Static (Item : Protocol_T) return Boolean
   is
   begin

      for I in Item.Intervals'Range loop

         if not Storage.Bounds.Singular (Item.Intervals(I)) then

            return False;

         end if;

      end loop;

      return True;

   end Static;


   function Basis (Item : Protocol_T) return Storage.Cell_List_T
   is

      Result : Storage.Cell_List_T (1 .. Item.Intervals'Length);
      Last : Natural := 0;
      -- The result will be Result(1 .. Last).

   begin

      for I in Item.Intervals'Range loop

         if not Storage.Bounds.Singular (Item.Intervals(I)) then
            -- This stack-height cell is not yet fully bounded.

            Last := Last + 1;

            Result(Last) := Programs.Stack_Height (I, Item.Program);

         end if;

      end loop;

      return Result(1 .. Last);

   end Basis;


   function Image (Item : Protocol_T) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      -- The result.

      Height : Storage.Cell_T;
      -- A stack-height cell.

   begin

      Result := To_Unbounded_String ("Stack");

      for I in Item.Intervals'Range loop

         Height := Programs.Stack_Height (I, Item.Program);

         Append (Result, ", ");

         Append (
            Result,
            Storage.Bounds.Image (
               Item => Item.Intervals(I),
               Name => Storage.Image (Height)));

      end loop;

      return To_String (Result);

   end Image;


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in     Protocol_T;
      Giving :    out Calling.Protocol_Ref)
   is
      use type Storage.Bounds.Interval_T;

      Height : Storage.Cell_T;
      -- A stack-height cell.

      New_Bounds : Storage.Bounds.Interval_T;
      -- The range of values for a stack-height cell under the Bounds.

      Result : Protocol_Ref := null;
      -- The new Stacked Protocol, if one is created.
      -- Initially we have not yet created one.

   begin

      for I in Upon.Intervals'Range loop

         Height := Programs.Stack_Height (
            Index  => I,
            Within => Upon.Program);

         New_Bounds :=  Storage.Bounds.Interval (
            Cell  => Height,
            Under => Bounds);

         if Upon.Intervals(I) <= New_Bounds then
            -- The new bounds are no better than the old ones.

            null;

         else
            -- The new bounds are sharper than the old ones at
            -- least at one end (min or max).

            if Result = null then
               -- This was the first stack with better bounds.
               -- Create a new protocol:

               Result := new Protocol_T'Class'(Protocol_T'Class (Upon));

            end if;

            Result.Intervals(I) := Upon.Intervals(I) and New_Bounds;

            if Storage.Bounds.Void (Result.Intervals(I)) then

               Output.Warning (
                    "Conflicting stack-height bounds"
                  & Output.Field_Separator
                  & Programs.Stack_Name (Index => I, Within => Upon.Program));

               -- TBA discard Result.

               raise Flow.False_Path;

            end if;

         end if;

      end loop;

      Giving := Calling.Protocol_Ref (Result);
      -- Null if no better bounds were found.

   end Apply;


end Calling.Stacked;
