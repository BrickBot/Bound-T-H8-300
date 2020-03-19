-- Programs.Call_Sets (decl)
--
-- Small sets of calls (Call_T).
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
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: programs-call_sets.ads,v $
-- Revision 1.2  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2008-02-18 12:45:53  niklas
-- First version, for BT-CH-0112.
--


with Bounded_Vectors;
with Flow;


package Programs.Call_Sets is


   type Bounded_Set_T (Max_Size : Positive) is private;
   --
   -- A set of calls, containing at most Max_Size calls.


   procedure Add (
      Call : in     Call_T;
      To   : in out Bounded_Set_T);
   --
   -- Adds the Call To the set, if not already there.
   -- No effect if the Call is already in the set.


   procedure Remove (
      Call : in     Call_T;
      From : in out Bounded_Set_T);
   --
   -- Removes the Call From the set.
   -- No effect if the Call was not in the set.


   function To_List (Set : Bounded_Set_T) return Call_List_T;
   --
   -- All calls in the Set, in some unspecified order.


   function Nodes (Set : Bounded_Set_T) return Flow.Node_List_T;
   --
   -- The nodes that contain the calls in the Set, in some
   -- unspecified order.


   function All_Calls (From : Subprogram_T) return Bounded_Set_T;
   --
   -- The set of all calls From the given subprogram. The result
   -- contains all the calls, but is mutable so some or all of
   -- these calls can be removed later.


   function No_Calls (From : Subprogram_T) return Bounded_Set_T;
   --
   -- The null set of calls From the given subprogram. The result
   -- is the empty set, but with room to add any or all of the
   -- calls From this subprogram.


private


   package Bounded_Call_Vectors is new Bounded_Vectors (
      Index_Type   => Positive,
      Element_Type => Call_T,
      Vector_Type  => Call_List_T);


   type Bounded_Set_T (Max_Size : Positive) is record
      Vector : Bounded_Call_Vectors.Bounded_Vector (
                  First => 1,
                  Last  => Max_Size);
   end record;


end Programs.Call_Sets;
