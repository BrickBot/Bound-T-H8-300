-- Programs.Call_Sets (body)
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
-- $Log: programs-call_sets.adb,v $
-- Revision 1.2  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2008-02-18 12:45:52  niklas
-- First version, for BT-CH-0112.
--


package body Programs.Call_Sets is


   use Bounded_Call_Vectors;


   procedure Add (
      Call : in     Call_T;
      To   : in out Bounded_Set_T)
   is
   begin

      Add (Value => Call, To => To.Vector);

   end Add;


   procedure Remove (
      Call : in     Call_T;
      From : in out Bounded_Set_T)
   is
   begin

      Drop_First (Value => Call, From => From.Vector);

   end Remove;


   function To_List (Set : Bounded_Set_T) return Call_List_T
   is
   begin

      return To_Vector (Set.Vector);

   end To_List;


   function Nodes (Set : Bounded_Set_T) return Flow.Node_List_T
   is
   begin

      return Nodes (To_List (Set));

   end Nodes;


   function All_Calls (From : Subprogram_T) return Bounded_Set_T
   is

      Calls : constant Call_List_T := Calls_From (Caller => From);
      -- All the calls, as a list.

      Set : Bounded_Set_T := No_Calls (From);
      -- The set, to hold the calls.

   begin

      for C in Calls'Range loop

         Add (Call => Calls(C), To => Set);

      end loop;

      return Set;

   end All_Calls;


   function No_Calls (From : Subprogram_T) return Bounded_Set_T
   is

      Num_Calls : constant Natural := Number_Of_Calls_From (Caller => From);
      -- The total number of calls.

      Set : Bounded_Set_T (Max_Size => Positive'Max (1, Num_Calls));
      -- A set able to hold that many calls.
      -- Default initialized to the empty set.

   begin

      return Set;

   end No_Calls;


end Programs.Call_Sets;
