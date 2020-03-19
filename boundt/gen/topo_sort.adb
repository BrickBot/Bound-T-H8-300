-- Topo_Sort (body)
--
-- Topological sorting.
-- Reference: D.Knuth, Fundamental Algorithms, 1969, page 259.
-- Author: Niklas Holsti, Space Systems Finland, 2000.
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: topo_sort.adb,v $
-- Revision 1.5  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.4  2004-04-25 07:53:26  niklas
-- First Tidorum version. Handling duplicates.
--
-- Revision 1.3  2000/08/18 18:02:43  holsti
-- Range of List_Ref_T increased to include Elements.
--
-- Revision 1.2  2000/07/12 20:41:24  holsti
-- Added Elements parameter for disconnected or singleton graphs.
--
-- Revision 1.1  2000/05/07 12:40:02  holsti
-- First version
--


function Topo_Sort (
   Elements : Element_List;
   Pairs    : Pair_List)
return Element_List
is

   -- Principle of Operation:
   --
   -- The algorithm has two phases: addition and subtraction.
   --
   -- In the addition phase, the given Elements and Pairs are entered into
   -- an internal data structure that contains the following:
   --
   -- > The set of all the elements mentioned in Elements or Pairs.
   --
   -- > For each such element E:
   --
   --   - The number of predecessor elements L, that is, the number of
   --     pairs P in which Lesser(P) = L and Greater(P) = E.
   --
   --   - The list of successor elements S, formed by listing S = Greater(P)
   --     for all pairs P where Lesser(P) = E.
   --
   -- If Pairs contains duplicates, that is two or more pairs with the
   -- same Lesser and same Greater elements, these are counted separately
   -- in the number of predecessors and create as many duplicate entries
   -- in the successor list. Thus, actually we count and list the incoming
   -- and outgoing arcs, rather than predecessor and successor elements.
   --
   -- In the subtraction phase, we remove one by one any element that has
   -- no predecessors and is therefore a "root" or minimal element in the
   -- Pair order. The removed elements become the sorted element list. When
   -- an element R is found to be a root (number of predecessors = 0), it
   -- is appended to the sorted list and removed from the internal data
   -- structure in the following way:
   --
   -- > Since R has no predecessors, it does not appear in the successor
   --   list of any other element. Thus those lists need no update.
   --
   -- > Since R is about to be removed from the set, it should no longer be
   --   counted as a predecessor of its successors. Therefore the successor
   --   list of R itself is scanned and for every successor element S the
   --   predecessor count of S is decremented. If the result is zero, S is
   --   a new root element.
   --
   -- The set of root elements is maintained as a queue, although any order
   -- could be used (all root elements are by definition incomparable and
   -- could be emitted in any order).


   --
   --    The set of all elements:
   --


   Max_Elements : constant Natural := Elements'Length + 2 * Pairs'Length;
   --
   -- The maximum number of elements.
   -- Each component of Elements can define one new element.
   -- Each component of Pairs can define two new elements.


   subtype Loc_T is Natural range 0 .. Max_Elements;
   --
   -- Refers to an element in the local element-set if positive.
   -- Refers to no element if zero.


   Last_Loc : Loc_T := 0;
   --
   -- Refers to the last element found, which has the highest index.
   -- Zero if no element found yet.


   subtype Index_T is Loc_T range 1 .. Loc_T'last;
   --
   -- Refers to an element in the local element-set.
   -- The valid range is 1 .. Last_Loc.


   Elems : Element_List (Index_T);
   --
   -- All the elements, in the order they happen to be found in Elements
   -- and Pairs. The valid ones are Elems(1 .. Last_Loc). There are no
   -- duplicates in the list.


   --
   --    Predecessor count
   --


   Pred_Count : array (Index_T) of Natural;
   --
   -- For each element, the number of predecessor elements (in fact,
   -- the number of pairs in which this element is the Greater half).


   --
   --    Successor lists
   --


   Max_List_Nodes : constant Natural := Pairs'Length;
   --
   -- Maximum number of successor list nodes.
   -- Each component of Pairs defines a successor node.


   subtype List_Ref_T is Natural range 0 .. Max_List_Nodes;
   --
   -- Refers to a list-pool node, or none if zero.


   No_List : constant List_Ref_T := 0;
   --
   -- The null list reference.


   subtype List_Index_T is List_Ref_T range 1 .. List_Ref_T'last;
   --
   -- The non-null list references.


   Loc  : array (List_Index_T) of Loc_T;
   Next : array (List_Index_T) of List_Ref_T;
   --
   -- These two arrays form the successor list pool.
   -- For a list node L (List_Ref_T), Loc(L) identifies the element
   -- in the list node, and Next(L) leads to the next node (if not
   -- null).


   Last_List : List_Ref_T := 0;
   --
   -- The last used location in the list pool (Loc, Next).
   -- Zero if no locations used yet.


   Succ_List : array (Loc_T) of List_Ref_T;
   --
   -- For each element, refers to the list of the element's direct
   -- successors (in fact the list of Greater elements for each Pair
   -- in which the given is the Lesser one).
   --
   -- No_List if there are no successors.


   --
   --    The resulting sorted list:
   --


   subtype Pos_T is Natural range 0 .. Max_Elements;
   --
   -- A position in the sorted result.
   -- Zero means before the first element.


   Result : Element_List (Pos_T range 1 .. Max_Elements);
   --
   -- Accumulates the result, sorted in topological order.


   Last_Result : Pos_T := 0;
   --
   -- Result(1 .. Last_Result) are valid.


   Last_Subtracted : Pos_T := 0;
   --
   -- The last Result element that has been subtracted from the internal
   -- data structure (predecessor counts and successor lists).
   -- The slice Result(1 .. Last_Subtracted) is fully finished.
   -- The slice Result(Last_Subtracted + 1 .. Last_Result) contains
   -- elements that have been identified as subtractable (no predecessors)
   -- but have not yet been subtracted.


   Pos_Loc : array (Pos_T range 1 .. Max_Elements) of Index_T;
   --
   -- The locations in Elems of the result elements.
   -- The valid part is Pos_Loc(1 .. Last_Result).


   procedure Locate (
      Elem : in     Element;
      Loc  :    out Loc_T)
   --
   -- Adds Elem to Elems, if not already there.
   -- Anyway, returns its location in Loc.
   --
   is
   begin

      for I in Elems'First .. Last_Loc loop

         if Elems(I) = Elem then
            -- Element was already listed.

            Loc := I;

            return;

          end if;

      end loop;

      -- The element is a new one.

      Last_Loc := Last_Loc + 1;
      Loc      := Last_Loc;

      Elems     (Loc) := Elem;
      Pred_Count(Loc) := 0;
      Succ_List (Loc) := No_List;

   end Locate;


   procedure Push (
      This : in     Loc_T;
      Onto : in out List_Ref_T)
   --
   -- Pushes This on top (head) of the Onto list.
   --
   is
   begin

      Last_List := Last_List + 1;

      Loc (Last_List) := This;
      Next(Last_List) := Onto;

      Onto := Last_List;

   end Push;


   procedure Add_Result (Loc : in Loc_T)
   --
   -- Adds Elems(Loc) to the Result list.
   --
   is
   begin

       Last_Result := Last_Result + 1;

       Result (Last_Result) := Elems(Loc);
       Pos_Loc(Last_Result) := Loc;

   end Add_Result;


   Unused : Loc_T;
   --
   -- An unused output from Locate, for Elements.


   Less, Great : Loc_T;
   --
   -- The locations of the Lesser and Greater elements of a Pair.


   Subtract : Loc_T;
   --
   -- The root element being subtracted from the internal data structures.


   L : List_Ref_T;
   --
   -- One node (tail) in the successor list of Subtract.


   Succ : Loc_T;
   --
   -- The location of the successor L.


begin  -- Topo_Sort

   --
   --    Addition phase:
   --

   -- Enter all the given elements:

   for E in Elements'Range loop

      Locate (Elem => Elements(E), Loc => Unused);

   end loop;

   -- Scan all order-pairs, count predecessors, list successors:

   for P in Pairs'range loop

      -- Record the lesser and greater elements as follows:

      Locate (Elem => Lesser (Pairs(P)), Loc => Less );
      Locate (Elem => Greater(Pairs(P)), Loc => Great);

      Pred_Count(Great) := Pred_Count(Great) + 1;

      Push (This => Great, Onto => Succ_List(Less));
         
   end loop;

   --
   --    Subtraction phase:
   --

   -- Collect the initial root queue:

   for E in Elems'First .. Last_Loc loop

      if Pred_Count(E) = 0 then

         Add_Result (E);

      end if;

   end loop;

   -- Emit in linear order, subtracting predecessor-less elements
   -- one by one and decreasing predecessor counts as predecessors are
   -- emitted:

   while Last_Subtracted < Last_Result loop

      -- There is a Result element that has not yet been subtracted.

      Subtract := Last_Subtracted + 1;

      -- Remove this element from its successor's pred-counts:

      L := Succ_List(Pos_Loc(Subtract));

      while L /= No_List loop

         Succ := Loc(L);

         Pred_Count(Succ) := Pred_Count(Succ) - 1;

         if Pred_Count(Succ) = 0 then
            -- This is a new root. Throw it in the Result.

            Add_Result (Succ);

         end if;

         -- Go on to next successor:

         L := Next(L);

      end loop;

      -- This element has been subtracted:

      Last_Subtracted := Subtract;
      
   end loop;

   return Result (Result'First .. Last_Result);

end Topo_Sort;
