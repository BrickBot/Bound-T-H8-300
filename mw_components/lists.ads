-- GENERIC PACKAGE FOR LISTS WITH POINTERS
   ---------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jun  8 11:43:17 1998
-- Update Count    : 6

-- Revision : 28-May-1998 by Mats Weber, made full view of type List limited.
-- Revision :  9-FEB-1989 by Mats Weber, added procedures MOVE_BEFORE and MOVE_AFTER.
-- Revision : 10-JAN-1989 by Mats Weber, removed the list parameter from
--                                       procedure ASSIGN(LIST, POINTER, ITEM_TYPE).
-- Revision : 23-NOV-1988 by Mats Weber, added procedure ASSIGN(LIST, POINTER, ITEM_TYPE).
-- Revision : 31-AUG-1988 by Mats Weber, added procedure SWAP and removed procedure MOVE.
-- Revision : 11-MAY-1988 by Mats Weber, added paramater DIRECTION to procedure REMOVE.

-- Creation : 26-FEB-1988 by Mats Weber.


generic
   type Item_Type is private;
   type Count is range <>;      -- must include 0
package Lists is
-------------

   type List is limited private;
   ---------

   type Pointer is private;
   ------------

   Null_Pointer : constant Pointer;
      -- Points to no item in no list.
      -- It is the initial value of the type POINTER.

   -- Calling a procedure or a function of this package with a list and a
   -- pointer that points into another list or an invalid pointer will
   -- produce unpredictable effects.

   -- Invalid pointers are created by removing the items to which they point
   -- or by destroying the entire list into which they point.

   -- NULL_POINTER is not an invalid pointer; procedures or functions called
   -- with NULL_POINTER as a parameter will raise the exception POINTER_IS_NULL,
   -- except where explicitly stated otherwise.


   type Scan_Direction is (Forward, Backward);

   subtype Natural_Count is Count range 0 .. Count'Last;


   procedure Insert_Before (Item   : in Item_Type;
                            Into   : in out List;
                            Before : in Pointer);
      -- If BEFORE = HEAD(INTO), then ITEM will replace the head of INTO,
      -- in particular if INTO is empty and BEFORE = NULL_POINTER.

   procedure Insert_After (Item  : in Item_Type;
                           Into  : in out List;
                           After : in Pointer);
      -- If AFTER = TAIL(INTO), then ITEM will replace the tail of INTO,
      -- in particular if INTO is empty and AFTER = NULL_POINTER.


   procedure Insert_At_Head (Item : in Item_Type;
                             Into : in out List);

   procedure Insert_At_Tail (Item : in Item_Type;
                             Into : in out List);


   procedure Remove (Position     : in out Pointer;
                     From         : in out List;
                     Direction    : in Scan_Direction := Forward);

   procedure Remove (Position     : in out Pointer;
                     From         : in out List;
                     Removed_Item : out Item_Type;
                     Direction    : in Scan_Direction := Forward);
      -- If LENGTH(FROM) = 1,     then POSITION will be set to NULL_POINTER.
      -- If DIRECTION = FORWARD,  then POSITION will be set to NEXT(POSITION).
      -- If DIRECTION = BACKWARD, then POSITION will be set to PREVIOUS(POSITION).


   procedure Remove_Head (From         : in out List);
   procedure Remove_Head (From         : in out List;
                          Removed_Item : out Item_Type);
      -- Will raise LIST_EMPTY if FROM is empty.

   procedure Remove_Tail (From         : in out List);
   procedure Remove_Tail (From         : in out List;
                          Removed_Item : out Item_Type);
      -- Will raise LIST_EMPTY if FROM is empty.


   function Element (Position : Pointer) return Item_Type;
      -- Returns the item at POSITION.

   procedure Assign (Position : in Pointer; Value : in Item_Type);
      -- Changes the value of the element at POSITION.


   function Head (Of_List : List) return Pointer;
   function Tail (Of_List : List) return Pointer;
      -- Will return NULL_POINTER if OF_LIST is empty.

   function Next     (Position : Pointer) return Pointer;
   function Previous (Position : Pointer) return Pointer;
      -- The conditions NEXT(TAIL(L)) = HEAD(L) and
      -- PREVIOUS(HEAD(L)) = TAIL(L) hold for all nonempty lists.

   function Length (Of_List : List) return Natural_Count;

   function Empty (The_List : List) return Boolean;

   function "=" (Left, Right : List) return Boolean;
      -- Returns true if and only if LEFT and RIGHT have the same elements
      -- in the same sequence and with the same head and the same tail.


   generic
      with procedure Action (Item : in Item_Type);
   procedure Traversal (In_List   : in List;
                        Direction : in Scan_Direction := Forward);

   generic
      with procedure Modify (Item : in out Item_Type);
   procedure Update_All (In_List   : in out List;
                         Direction : in Scan_Direction := Forward);


   function Search (Item      : Item_Type;
                    Within    : List;
                    Direction : Scan_Direction := Forward) return Pointer;
      -- Will raise ITEM_NOT_FOUND if ITEM is not in WITHIN.

   generic
      with function Condition (Item : Item_Type) return Boolean;
   function Scan (Within    : List;
                  Direction : Scan_Direction := Forward) return Pointer;
      -- Will raise ITEM_NOT_FOUND if no item in WITHIN satisfies CONDITION.


   procedure Assign (Object : in out List; Value : in List);
      -- Copies VALUE into OBJECT, destroying OBJECT if necessary.
      -- Pointers into VALUE do not become pointers into OBJECT.


   procedure Move_Before (From   : in out List;
                          Into   : in out List;
                          Before : in Pointer);
      -- Moves all elements in FROM into INTO before position BEFORE.
      -- If BEFORE = HEAD(INTO), then FROM will be inserted at the head of INTO,
      -- in particular if INTO is empty and BEFORE = NULL_POINTER.
      -- Pointers to elements of FROM become pointers to the same elements in INTO.
      -- FROM is emptied.

   procedure Move_After (From  : in out List;
                         Into  : in out List;
                         After : in Pointer);
      -- Moves all elements in FROM into INTO after position AFTER.
      -- If AFTER = TAIL(INTO), then FROM will be inserted at the tail of INTO,
      -- in particular if INTO is empty and AFTER = NULL_POINTER.
      -- Pointers to elements of FROM become pointers to the same elements in INTO.
      -- FROM is emptied.


   procedure Swap (Left, Right : in out List);
      -- Exchanges LEFT and RIGHT.
      -- Pointers to elements of LEFT become pointers to the
      -- same elements of RIGHT and vice versa.

   procedure Destroy (The_List : in out List);
      -- Removes all items in THE_LIST.


   List_Empty,
   Item_Not_Found,
   Pointer_Is_Null   : exception;

private

   type Cell;

   type Link is access Cell;

   type Cell is
      record
         Value          : Item_Type;
         Next, Previous : Link;
      end record;

   type List is limited
      record
         Head, Tail : Link;
         Length     : Natural_Count := 0;
      end record;

   type Pointer is new Link;

   Null_Pointer : constant Pointer := null;


   pragma Inline(Element, Head, Tail, Next, Previous, Length, Empty, Swap);

end Lists;
