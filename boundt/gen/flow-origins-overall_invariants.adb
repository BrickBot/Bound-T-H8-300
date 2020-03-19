-- Flow.Origins.Overall_Invariants (body)
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
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-origins-overall_invariants.adb,v $
-- Revision 1.4  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.3  2013/12/13 17:22:55  niklas
-- BT-CH-0262, addition: Corrections to new value-origin analysis.
--
-- Revision 1.2  2013/12/12 22:26:04  niklas
-- BT-CH-0262: Corrections to new value-origin analysis.
--
-- Revision 1.1  2013/12/08 22:05:57  niklas
-- BT-CH-0259: Storing value-origin analysis results in execution bounds.
--


with Flow.Computation;
with Flow.Origins.Opt;
with Output;
with Storage;


function Flow.Origins.Overall_Invariants (Map : Map_Ref)
return Storage.Cell_List_T
is
   use type Storage.Cell_T;

   Cells : Storage.Cell_List_T := All_Cells (Under => Map);
   -- Initially, all the cells considered in the value-origin analysis.
   -- At end, only the overall invariant cells are left, at indices
   -- Cells'First .. Last_Invariant.

   Last_Invariant : Natural := Cells'First - 1;
   -- The index of the last invariant cell in Cells.

   Returns : constant Step_List_T :=
      Flow.Computation.Final_Steps (Computation (Map).all);
   -- All the return steps.

   Origin : Origin_T;
   -- The origin found for a cell, at a return.

   Invariant : Boolean;
   -- Whether the cell is invariant: its final value at any return
   -- equals its initial value on entry.

begin

   if Returns'Length = 0 then

      Output.Warning ("Non-returning subprogram.");

   end if;

   Loop_Over_Cells : for C in Cells'Range loop

      Invariant := True;
      -- We know nothing to the contrary.

      -- Look at the origin of the cell at all return points.
      -- If they are all "Initial", the cell is invariant:

      Loop_Over_Returns: for R in Returns'Range loop

         Origin := Origin_After (
            Step => Returns(R),
            Cell => Cells(C),
            From => Map);

         case Origin.Kind is
         when Initial =>

            Invariant := Origin.Cell = Cells(C);

         when Assigned | Merged =>

            Invariant := False;

         end case;

         exit Loop_Over_Returns when not Invariant;

      end loop Loop_Over_Returns;

      if Invariant then
         -- This is an invariant cell (at all return points).

         if Opt.Trace_Invariant_Cells then

            Output.Trace (
                 "Invariant cell"
               & Output.Field_Separator
               & Storage.Image (Cells(C)));

         end if;

         Last_Invariant := Last_Invariant + 1;

         if Last_Invariant < C then
            -- Some earlier cells were not invariant.
            -- Move this cell to extend the contiguous list of
            -- invariant cells at the start of the array.

            Cells(Last_Invariant) := Cells(C);

         end if;

      end if;

   end loop Loop_Over_Cells;

   return Cells(Cells'First .. Last_Invariant);

end Flow.Origins.Overall_Invariants;
