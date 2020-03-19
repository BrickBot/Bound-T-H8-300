-- Interval_Maps (body)
--
-- Author: Niklas Holsti.
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: interval_maps.adb,v $
-- Revision 1.6  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.5  2008-11-08 17:39:44  niklas
-- Added function Next_Undefined.
--
-- Revision 1.4  2007/04/18 18:35:49  niklas
-- BT-CH-0057.
--
-- Revision 1.3  2007/03/29 12:51:48  niklas
-- BT-CH-0056.
--
-- Revision 1.2  2007/01/25 21:25:37  niklas
-- BT-CH-0043.
--
-- Revision 1.1  2004/04/24 17:17:59  niklas
-- First version.
--


with Ada.Text_IO;
with Unchecked_Deallocation;


package body Interval_Maps is


   -- A map is implemented as a binary tree where each node
   -- represents one point (interval -> value) in the map.


   type Node_T;

   type Node_Ref is access Node_T;

   --:dbpool Node_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Node_Ref'Storage_Pool use Node_Pool;


   type Node_T is record
      Interval : Interval_T;
      Value    : Value_Type;
      Before   : Node_Ref;
      After    : Node_Ref;
   end record;
   --
   -- One point in an interval map.
   --
   -- Interval
   --    The definition interval.
   -- Value
   --    The value of the map in Interval.
   -- Before
   --    The subtree of map-points, all of which have intervals
   --    less than (preceding) this Interval.
   -- After
   --    The subtree of map-points, all of which have intervals
   --    greater than (following) this Interval.
   --
   -- Together, a given Node and its Before and After subtrees cover
   -- a whole map, because no intervals in the map domain will intersect.


   type Map_Object_T is record
      Root : Node_Ref;
      Memo : Node_Ref;
   end record;
   --
   -- A map is represented by a binary tree of nodes.
   --
   -- Root
   --    The root of the tree. Null if the map is null (defined
   --    nowhere).
   -- Memo
   --    The node that was last used for the Value function.
   --    This is used to accelerate the Value function.
   --    Null if unknown.


   function Is_In (Index : Index_Type; Interval : Interval_T)
   return Boolean
   --
   -- Whether the index is in the interval.
   --
   is
   begin

      return Index in Interval.First .. Interval.Last;

   end Is_In;


   function Is_In (Index : Index_Type; Node : Node_Ref)
   return Boolean
   --
   -- Whether the index is in the interval of the node.
   -- False for a null node-ref.
   --
   is
   begin

      return
         Node /= null
         and then Index in Node.Interval.First .. Node.Interval.Last;

   end Is_In;


   function Intersects (Left, Right : Interval_T)
   return Boolean
   --
   -- Whether the two intervals have a non-null intersection.
   --
   is
   begin

      return Index_Type'Max (Left.First, Right.First)
          <= Index_Type'Min (Left.Last , Right.Last );

   end Intersects;


   function "<=" (Left, Right : Interval_T) return Boolean
   --
   -- Whether the Left interval is wholly contained in the Right one.
   --
   is
   begin

      return Left.First <= Right.First and Right.Last <= Left.Last;

   end "<=";


   procedure Unchecked_Discard
   is new Unchecked_Deallocation (
      Object => Node_T,
      Name   => Node_Ref);


   procedure Discard (Item : in out Node_Ref)
   is
   begin

      if Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   end Discard;


   procedure Unchecked_Discard
   is new Unchecked_Deallocation (
      Object => Map_Object_T,
      Name   => Interval_Map_T);


   procedure Discard (Item : in out Interval_Map_T)
   is
   begin

      if Deallocate then

         Unchecked_Discard (Item);

      else

         Item := null;

      end if;

   end Discard;


   procedure Discard_Subtree (Node : in out Node_Ref)
   --
   -- Discards all the nodes in the subtree rooted at Node,
   -- and also Node itself.
   --
   is
   begin

      if Node /= null then

         Discard_Subtree (Node.Before);

         Discard_Subtree (Node.After);

         Discard (Node);

      end if;

   end Discard_Subtree;


   procedure Discard_Map (Map : in out Interval_Map_T)
   --
   -- Discards all nodes in the Map and also Map itself.
   --
   is
   begin

      if Map /= null then

         Discard_Subtree (Map.Root);

      end if;

      Discard (Map);

   end Discard_Map;


   procedure Initialize (Map : in out Interval_Map_T)
   is
   begin

      Discard_Map (Map);

      Map := new Map_Object_T;

   end Initialize;


   function Contiguous (Left, Right : Interval_T) return Boolean
   --
   -- Whether the Left interval is immediately followed by the
   -- Right interval.
   --
   is
   begin

      return Left.Last < Index_Type'Last
         and then Left.Last + 1 = Right.First;

   end Contiguous;


   procedure Insert (
      Interval : in     Interval_T;
      Value    : in     Value_Type;
      Tree     : in out Node_Ref)
   --
   -- Inserts a new mapping Interval -> Value in its proper place in
   -- the Tree, under the assumed precondition that Interval intersects
   -- no interval already in the tree. However, the Interval may be
   -- contiguous with some interval already in the tree, and if the
   -- values are also the same, the existing interval is simply extended
   -- and no new node is inserted.
   --
   is
   begin

      if Tree = null then
         -- This is the first node in the tree.

         Tree :=
            new Node_T'(
               Interval => Interval,
               Value    => Value,
               Before   => null,
               After    => null);

      elsif Contiguous (Tree.Interval, Interval)
      and then Tree.Value = Value
      then
         -- We can simply extend Tree.Interval to higher indices.

         Tree.Interval.Last := Interval.Last;

      elsif Contiguous (Interval, Tree.Interval)
      and then Tree.Value = Value
      then
         -- We can simply extend Tree.Interval to lower indices.

         Tree.Interval.First := Interval.First;

      elsif Interval.Last < Tree.Interval.First then
         -- The new interval is completely before the interval
         -- in the root of the tree.

         Insert (Interval, Value, Tree.Before);

      elsif Interval.First > Tree.Interval.Last then
         -- The new interval is completely after the interval
         -- in the root of the tree.

         Insert (Interval, Value, Tree.After);

      else
         -- The new interval intersects the interval in the
         -- root of the tree: precondition violation.

         raise Program_Error;

      end if;

   end Insert;


   procedure Insert (
      Interval : in     Interval_T;
      Value    : in     Value_Type;
      Into     : in out Interval_Map_T)
   --
   -- Inserts a new map-point (Interval -> Value) into a map,
   -- under the assumed precondition that the new Interval does not
   -- intersect any interval already in the domain of the map.
   -- That is, the initial map does not define the mapping value
   -- for any index in Interval.
   --
   is
   begin

      Insert (Interval, Value, Into.Root);

   end Insert;


   procedure Delete (Node : in out Node_Ref)
   --
   -- Deletes a given node and moves its children into its place in the
   -- tree that contains the deleted node.
   --
   is

      Before : Node_Ref := Node.Before;
      After  : Node_Ref := Node.After;
      Scan   : Node_Ref;

   begin

      Discard (Node);

      if Before = null then

         Node := After;

      elsif After = null then

         Node := Before;

      else
         -- Difficult case: the node has both Before and After
         -- children. Insert one into the other to make a single
         -- tree that can replace the deleted Node.

         -- Insert Before in the After subtree as the leftmost
         -- node:

         Scan := After;

         while Scan.Before /= null loop

           Scan := Scan.Before;

         end loop;

         Scan.Before := Before;

         -- TBA: instead Insert (Node => After, Tree => Before) if
         -- this gives a better balance.

         Node := After;

      end if;

   end Delete;


   procedure Undefine (
      Interval : in     Interval_T;
      From     : in out Interval_Map_T)
   --
   -- Removes the given Interval from the definition domain of the
   -- given interval map, giving a map that defines no point in the
   -- given Interval but is otherwise the same as the initial map.
   -- Assumes as a precondition that Interval is not null.
   --
   -- Any existing interval in the map, that is wholly contained in the
   -- Interval to be undefined, is deleted from the map tree.
   -- Any existing interval in the map that intersects the Interval to
   -- be undefined is reduced to eliminate this intersection. If an
   -- existing interval wholly contains the Interval to be undefined and
   -- extends beyond it a both ends, the existing interval is split into
   -- two intervals, one for each end that remains after the Interval to
   -- be undefined is removed.
   --
   is

      procedure Undefine_Interval (Tree : in out Node_Ref)
      --
      -- Undefines the given Interval within the given Tree (possible null).
      --
      is

         Root : Interval_T;
         -- The interval of the root, Tree.

      begin

         if Tree /= null then

            Root := Tree.Interval;

            -- Undefine in subtrees:

            if Interval.First < Root.First then
               -- The interval may intersect some Before intervals.

               Undefine_Interval (Tree => Tree.Before);

            end if;

            if Interval.Last > Root.Last then
               -- The interval may intersect some After intervals.

               Undefine_Interval (Tree => Tree.After);

            end if;

            -- Undefine in root:

            if Interval.Last  < Root.First
            or Interval.First > Root.Last
            then
               -- The intervals do not intersect.

               null;

            elsif Interval.First <= Root.First
            and   Interval.Last  >= Root.Last
            then
               -- The root interval is fully undefined.

               Delete (Node => Tree);

            elsif Interval.First > Root.First
            and   Interval.Last  < Root.Last
            then
               -- The middle of the root interval is undefined.
               -- Split the root node:

               Insert (
                  Interval => (
                     First => Interval.Last + 1,
                     Last  => Root.Last),
                  Value  => Tree.Value,
                  Tree => Tree.After);

               Tree.Interval.Last := Interval.First - 1;

               -- TBA instead Insert the low end in Before and redefine
               -- Tree as the high end, if this gives better balance.

            elsif Interval.First >  Root.First
            and   Interval.Last  >= Root.Last
            then
               -- The high end is undefined, the low end is left.

               Tree.Interval.Last := Interval.First - 1;

            elsif Interval.Last  <  Root.Last
            and   Interval.First <= Root.First
            then
               -- The low end is undefined, the high end is left.

               Tree.Interval.First := Interval.Last + 1;

            end if;

        end if;

      end Undefine_Interval;


   begin

      if Interval.First <= Interval.Last then

         Undefine_Interval (Tree => From.Root);

      end if;

   end Undefine;


   procedure Set (
      Map  : in out Interval_Map_T;
      From : in     Interval_T;
      To   : in     Value_Type)
   is
   begin

      if From.First <= From.Last then

         Undefine (Interval => From, From => Map);

         Insert (
            Interval => From,
            Value    => To,
            Into     => Map);

      end if;

   end Set;


   procedure Find_Node (
      Covering : in     Index_Type;
      Within   : in     Interval_Map_T;
      Node     :    out Node_Ref)
   --
   -- Finds the node that covers the given index, within an
   -- interval map. Returns null if the index is not in the
   -- domain of the map.
   --
   is
   begin

      Node := Within.Root;

      while Node /= null loop

         if Covering < Node.Interval.First then

            Node := Node.Before;

         elsif Covering > Node.Interval.Last then

            Node := Node.After;

         else
            -- Hit!

            exit;

         end if;

      end loop;

   end Find_Node;


   procedure Find_Next_Node (
      After : in     Index_Type;
      Tree  : in     Node_Ref;
      Node  :    out Node_Ref;
      Next  :    out Index_Type)
   --
   -- Finds the node, in the given tree, that covers the least index
   -- that is greater than After. If such a node is found, also returns
   -- the index in Next.
   --
   -- Returns null if no indices > After are in the domain of the tree.
   -- In this case the value of Next is not defined.
   --
   -- The index After, itself, may or may not be in the domain.
   -- We assume that After < Index_Type'Last.
   --
   is

      Subnode : Node_Ref;
      -- The next covering node found in a subtree.

   begin

      Node := Tree;

      Next := Index_Type'Last;
      -- Dummy value.

      while Node /= null loop

         if After >= Node.Interval.Last then
            -- The next node must be in the After subtree, if anywhere.

            Node := Node.After;

         elsif After >= Node.Interval.First then
            -- Node.Interval.First <= After < Node.Interval.Last.
            -- This is the node we are looking for.

            Next := After + 1;

            exit;

         else
            -- After < Node.Interval.First.
            -- The next node may be in the Before subtree or
            -- it may be this Node itself.

            Find_Next_Node (
               After => After,
               Tree  => Node.Before,
               Node  => Subnode,
               Next  => Next);

            if Subnode /= null then
               -- The next node was found in the Before subtree.

               Node := Subnode;

            else
               -- The Before subtree defines no indices > After,
               -- so the next node is Node itself, in fact.

               Next := Node.Interval.First;

            end if;

            -- In either case we have the result:

            exit;

         end if;

      end loop;

   end Find_Next_Node;


   function Value (
      Map   : Interval_Map_T;
      Index : Index_Type)
   return Value_Type
   is

      Node : Node_Ref := Map.Memo;

   begin

      if not Is_In (Index, Node) then
         -- Memo is no help, must look up the node.

         Find_Node (Covering => Index, Within => Map, Node => Node);

      end if;

      if Node = null then

         raise Undefined_Value;

      end if;

      Map.Memo := Node;

      return Node.Value;

   end Value;


   function Value (
      Map  : Interval_Map_T;
      Over : Interval_T)
   return Value_Type
   is

      Node : Node_Ref := Map.Memo;

   begin

      if Over.First > Over.Last then
         -- The interval is empty.

         raise Undefined_Value;

      end if;

      -- Find the node that covers the first index in Over:

      if not Is_In (Over.First, Node) then
         -- The Memo does not cover the first index in the interval.
         -- We must look it up.

         Find_Node (Covering => Over.First, Within => Map, Node => Node);

         if Node /= null then

            Map.Memo := Node;

         end if;

      end if;

      -- Verify that the same node covers all of Over:

      if not Is_In (Over.Last, Node) then
         -- Either Node is null, i.e. Over.First was already
         -- undefined, or Over.Last is not covered by the same
         -- node, which means that it is either undefined or is
         -- defined by a different node, which means that some
         -- indices in Over are undefined or that Over.Last maps
         -- to a different value than Over.First.

         raise Undefined_Value;

      end if;

      return Node.Value;

   end Value;


   function First (Map : Interval_Map_T) return Index_Type
   is
      Node : Node_Ref := Map.Root;
   begin

      if Node = null then
         -- The map is defined nowhere.

         raise Undefined_Value;

      else
         -- The map is defined somewhere.
         -- Find the earliest definition interval:

         while Node.Before /= null loop

            Node := Node.Before;

         end loop;

         return Node.Interval.First;

      end if;

   end First;


   function Next (After : Index_Type; Map : Interval_Map_T)
   return Index_Type
   is

      Node : Node_Ref;
      -- The node that covers the next index, if any.

      Index : Index_Type;
      -- The next index itself.

   begin

      if After = Index_Type'Last then

         raise Undefined_Value;

      elsif Is_In (After + 1, Map.Memo) then

         return After + 1;

      else

         Find_Next_Node (
            After => After,
            Tree  => Map.Root,
            Node  => Node,
            Next  => Index);

         if Node = null then

            raise Undefined_Value;

         end if;

         Map.Memo := Node;

         return Index;

      end if;

   end Next;


   function Last (Map : Interval_Map_T) return Index_Type
   is
      Node : Node_Ref := Map.Root;
   begin

      if Node = null then
         -- The map is defined nowhere.

         raise Undefined_Value;

      else
         -- The map is defined somewhere.
         -- Find the latest definition interval:

         while Node.After /= null loop

            Node := Node.After;

         end loop;

         return Node.Interval.Last;

      end if;

   end Last;


   function Defined (Map : Interval_Map_T; Index : Index_Type)
   return Boolean
   is
   begin

      return Defined (Map, (First => Index, Last => Index));

   end Defined;


   function Defined (
      Map    : Interval_Map_T;
      Within : Interval_T)
   return Boolean
   is

      Node : Node_Ref := Map.Memo;

   begin

      if Node /= null
      and then Intersects (Within, Node.Interval)
      then
         -- The Memo defines some part of Within.

         return True;

      else
         -- The Memo is no help.
         -- Search the map for the answer.

         Node := Map.Root;

         while Node /= null loop

            exit when Intersects (Within, Node.Interval);

            -- Since Within does not intersect Node.Interval, it
            -- must lie entirely Before or After the latter.

            if Within.First < Node.Interval.First then

               Node := Node.Before;

            else

               Node := Node.After;

            end if;

         end loop;

         return Node /= null;

      end if;

   end Defined;


   function Fully_Defined (
      Map  : Interval_Map_T;
      Over : Interval_T)
   return Boolean
   --
   -- Whether the Map is defined for all indices in the interval.
   -- Note that a True value does not mean that the Map has the
   -- same value for all indices in the interval.
   -- True is returned if the interval is empty.

   is

      Next : Index_Type := Over.First;
      -- The index to be checked for definition.

      Int : Interval_T;
      -- The defining interval that spans Next.

   begin

      while Next <= Over.Last loop
         -- We have verified so far that Over.First .. Next - 1
         -- is fully defined.

         Int := Span (Around => Next, Within => Map);

         exit when Over.Last <= Int.Last;
         -- The rest of Over is in the span Int, so Over is
         -- fully defined.

         Next := Int.Last + 1;
         -- Check the rest of Over that exceeds Int.

      end loop;

      return True;

   exception

   when Undefined_Value =>
      -- Some Span call found that the Map is not
      -- defined for the Next index.

      return False;

   end Fully_Defined;


   function Span (
      Around : Index_Type;
      Within : Interval_Map_T)
   return Interval_T
   --
   -- The maximal interval that contains the index Around and
   -- maps to the same value as Around.
   -- Raises Undefined_Value if the map is not defined for Around.
   is

      Node : Node_Ref := Within.Memo;

   begin

      if not Is_In (Around, Node) then
         -- The Memo is no help; look it up.

         Find_Node (Covering => Around, Within => Within, Node => Node);

         if Node /= null then

            Within.Memo := Node;

         end if;

      end if;

      if not Is_In (Around, Node) then

         raise Undefined_Value;

      end if;

      return Node.Interval;

   end Span;


   function Next_Undefined (After : Index_Type; Map : Interval_Map_T)
   return Index_Type
   is

      Now_After : Index_Type := After;
      -- A point in our search.

      Candidate : Index_Type;
      -- A possible result.

   begin

      while Now_After < Index_Type'Last loop

         Candidate := Now_After + 1;

         if not Defined (Map => Map, Index => Candidate) then

            return Candidate;

         else

            Now_After := Span (Around => Candidate, Within => Map).Last;

         end if;

      end loop;

      raise Undefined_Value;

   end Next_Undefined;


   procedure Show_Tree (
      Tree   : in Node_Ref;
      Margin : in Natural)
   is
      use Ada.Text_IO;

      New_Margin : constant Natural := Margin + 1;

   begin

      if Tree = null then

         for N in 1 .. Margin loop Put ("|  "); end loop;

         Put_Line ("null");

      else

         Show_Tree (Tree.Before, New_Margin);

         for N in 1 .. Margin loop Put ("|  "); end loop;

         Put_Line (
              '['
            & Index_Type'Image (Tree.Interval.First)
            & " .."
            & Index_Type'Image (Tree.Interval.Last)
            & " ] ->"
            & Image (Tree.Value));

         Show_Tree (Tree.After, New_Margin);

     end if;

   end Show_Tree;


   procedure Show (Item : in Interval_Map_T)
   is
      use Ada.Text_IO;
   begin

      Put_Line ("Interval Map Tree:");

      Show_Tree (Item.Root, 0);

      Put_Line ("end of Interval Map Tree.");

   end Show;


end Interval_Maps;
