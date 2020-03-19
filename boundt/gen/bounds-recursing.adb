-- Bounds.Recursing (body)
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
-- $Date: 2015/10/24 20:05:46 $
--
-- $Log: bounds-recursing.adb,v $
-- Revision 1.3  2015/10/24 20:05:46  niklas
-- Moved to free licence.
--
-- Revision 1.2  2005-06-12 07:30:24  niklas
-- Changed the display of calls from A->B to A=>B, to avoid
-- confusion with '-' used in approximate line-numbers.
--
-- Revision 1.1  2005/02/16 21:11:39  niklas
-- BT-CH-0002.
--


with Output;


package body Bounds.Recursing is


   procedure Report_Cycle (
      Recursive     : in Programs.Subprogram_Set_T;
      Non_Recursive : in Programs.Subprogram_List_T)
   is
      use type Programs.Subprogram_T;

      Max_Cycle_Length : constant Positive :=
         Programs.Cardinality (Recursive) + 1;
      --
      -- Maximum number of subprograms in recursion cycle (all
      -- subprograms, one subprogram twice).

      Cycles : Programs.Subprogram_Set_T;
      --
      -- Set that contains subprograms that are in Recursive but not
      -- in Non_Recursive. All these subprograms are members of some
      -- recursion cycle, but not necessarily of the same cycle.

      Recursion: Programs.Subprogram_List_T (1 .. Max_Cycle_Length);
      --
      -- Subprograms that call each other so that Recursion(I) calls
      -- Recursion(I+1).
      -- The range 1 .. Last of the array is used.
      -- The range First_In_Cycle .. Last contains subprograms in
      -- a recursion cycle.

      Subprogram : Programs.Subprogram_T;
      --
      -- The subprogram we are currently examining.

      Last           : Positive range Recursion'Range := 1;
      First_In_Cycle : Positive range Recursion'Range := 1;
      --
      -- See Recursion.

      Found : Boolean := False;
      --
      -- Indicates that a recursion cycle is found.

      Internal_Error : exception;
      --
      -- Subprograms in Cycles did not call each other in cycle.


      function Any_Callee (
         From : Programs.Subprogram_T;
         Into : Programs.Subprogram_Set_T)
      return Programs.Subprogram_T
      --
      -- Returns some callee of the given subprogram that is
      -- also a member of the given set.
      -- Raises Internal_Error if there is no such callee.
      --
      is
         All_Calls : constant Programs.Call_List_T :=
            Programs.Calls (From => From, Into => Into);
         -- All the calls from the given subprogram into the
         -- given subprogram set.
      begin

         if All_Calls'Length = 0 then

            raise Internal_Error;

         end if;

         return Programs.Callee (All_Calls(All_Calls'First));

      end Any_Callee;


   begin  -- Report_Recursion_Cycle

      -- Compute Cycles as the set difference, Recursive - Non_Recursive:

      Programs.Add (To => Cycles, Adding => Recursive);

      for S in Non_Recursive'Range loop

         Programs.Remove (From => Cycles, Removing => Non_Recursive(S));

      end loop;

      -- Start from any subprogram in the set Cycles:

      declare
         Cycles_List : Programs.Subprogram_List_T :=
            Programs.To_List (Cycles);
      begin

         Subprogram := Cycles_List(Cycles_List'First);

      end;

      -- Follow the call path until we find a subprogram that
      -- we have found before:

      while not Found loop

         Recursion(Last) := Subprogram;

         Subprogram := Any_Callee (
            From => Subprogram,
            Into => Cycles);

         for I in Recursion'First .. Last loop

            if Recursion(I) = Subprogram then

               Found := True;
               First_In_Cycle := I;

               exit;

            end if;

         end loop;

         if Last >= Recursion'Last then
            -- This should be impossible.

            raise Internal_Error;

         end if;

         Last := Last+1;

      end loop;

      -- We have found a cycle Subprogram => Subprogram.

      Recursion(Last) := Subprogram;

      -- Report the problem, and the cycle:

      Output.Error (
         Locus => Programs.Locus (Subprogram),
         Text  => "Recursion detected" );

      for S in First_In_Cycle .. Last-1 loop

         Output.Result (
            Key   => "Recursion_Cycle",
            Locus => Programs.Locus (Recursion(S)),
            Text  => "Calls " & Programs.Name (Recursion(S+1)));

      end loop;

   exception

   when X : others =>

      Output.Exception_Info (
         Text       => "Bounds.Recursing.Report_Cycle",
         Occurrence => X);

   end Report_Cycle;


end Bounds.Recursing;
