-- Storage.Volatiles (body)
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
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: storage-volatiles.adb,v $
-- Revision 1.2  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2013-02-12 08:47:20  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--


with Ada.Containers.Vectors;
with Output;
with Processor;
with Storage.Opt;


package body Storage.Volatiles is


   type Cell_Range_T is record
      From : Cell_Spec_Ref;
      To   : Cell_Spec_Ref;
   end record;
   --
   -- A range of storage that extends From the first bit of
   -- a given cell, up To and including the last bit of another
   -- given cell, and contains all bits "in between" (as defined
   -- by target-specific functions).
   --
   -- The cell-specs are references since we do not want to
   -- assume that Cell_Spec_T is a definite type.


   package Cell_Range_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Cell_Range_T);
   --
   -- Vectors of Cell_Range_T objects.
   -- Predefined equality is used.
   --
   type Cell_Range_Vector_T is new Cell_Range_Vectors.Vector with null record;


   Volatile_Ranges : Cell_Range_Vector_T;
   --
   -- All the cell-ranges marked as volatile.
   -- The order is arbitrary.
   -- The ranges may overlap.
   -- Initially empty.


   function In_Range (
      Cell     : Processor.Cell_Spec_T;
      From, To : Processor.Cell_Spec_T)
   return Boolean
   --
   -- Whether the Cell is in the range From .. To.
   -- Also emits a Note, if so.
   --
   is

      Yes : constant Boolean :=
         Processor.Cell_In_Range (
            Cell => Cell,
            From => From,
            To   => To);

   begin

      if Yes then

         Output.Note (
              "Cell "
            & Processor.Image (Cell)
            & " is in volatile range "
            & Processor.Image (From)
            & " .. "
            & Processor.Image (To));

      end if;

      return Yes;

   end In_Range;


   procedure Mark_Range (
      From, To    : in     Processor.Cell_Spec_T;
      Valid_Range :    out Boolean)
   is

      Cell : Cell_T;
      -- An existing cell, possibly to be marked volatile.

   begin

      Valid_Range := Processor.Valid_Cell_Range (From, To);

      if Valid_Range then

         if Opt.Trace_Volatile then

            Output.Trace (
                 "Volatile range"
               & Output.Field_Separator
               & Processor.Image (From)
               & Output.Field_Separator
               & Processor.Image (To));

         end if;

         -- Add the new range to the list:

         Append (
           Container => Volatile_Ranges,
           New_Item  => Cell_Range_T'(
              From => new Processor.Cell_Spec_T'(From),
              To   => new Processor.Cell_Spec_T'(To  )));

         -- If any existing cell lies in the new range, mark
         -- the cell as volatile:

         for C in Cell_Index_T'First .. Cell_Index_T (Number_Of_Cells) loop

            Cell := Cell_At (Index => C);

            if In_Range (Spec_Of (Cell), From, To) then

               Mark_As_Volatile (Cell);

            end if;

         end loop;

      end if;

   end Mark_Range;


   function In_Volatile_Range (Cell : Processor.Cell_Spec_T)
   return Boolean
   is

      Vola : Cell_Range_T;
      -- One of the volatile ranges.

   begin

      -- Check if the Cell is in any of the Volatile_Ranges:

      for V in First_Index (Volatile_Ranges) .. Last_Index (Volatile_Ranges)
      loop

         Vola := Element (Volatile_Ranges, V);

         if In_Range (Cell, Vola.From.all, Vola.To.all) then
            -- The Cell is in this range.

            return True;

         end if;

      end loop;

      -- The Cell is not in any of the Volatile_Ranges.

      return False;

   end In_Volatile_Range;


end Storage.Volatiles;
