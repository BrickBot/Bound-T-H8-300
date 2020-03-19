-- GENERIC PACKAGE FOR SETS OF ANY BASE TYPE
   -----------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jun  8 11:34:55 1998
-- Update Count    : 4

-- Revision : 31-AUG-1988 by Mats Weber, added procedure SWAP.
-- Revision : 28-JUN-1988 by Mats Weber, renamed package from GENSETS to GENERAL_SETS.
-- Revision : 17-APR-1988 by Mats Weber, removed functions returning SETs because they generate
--                                       garbage.
-- Revision : 11-APR-1988 by Mats Weber, added procedures INTERSECT(SET, SET, SET),
--                                       INTERSECT(LIST, SET, SET) and INTERSECT(SET, LIST, SET).
-- Revision :  7-DEC-1987 by Mats Weber, added function MEMBER(ELEMENT_TYPE, LIST).
-- Revision :  4-NOV-1987 by Mats Weber, added generic formal type COUNT.
-- Revision : 27-MAY-1986 by Mats Weber, renamed DESTROY to EMPTY.
-- Revision : 11-DEC-1985 by Mats Weber.

-- Creation : 20-JUN-1985 by S. Mourtada.


with Bags;

generic
   type Element_Type is private;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   type Count is range <>;   -- must include 0
package General_Sets is
--------------------

   type Set is limited private;
   --------

   subtype Natural_Count  is Count range 0 .. Count'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;

   type List is array (Positive_Count range <>) of Element_Type;


   function "=" (Left, Right : Set) return Boolean;

   -- Strict inclusion
   function "<" (Left : Set;  Right : Set)  return Boolean;
   function "<" (Left : Set;  Right : List) return Boolean;
   function "<" (Left : List; Right : Set)  return Boolean;

   function ">" (Left : Set;  Right : Set)  return Boolean;
   function ">" (Left : Set;  Right : List) return Boolean;
   function ">" (Left : List; Right : Set)  return Boolean;

   -- Inclusion
   function "<=" (Left : Set;  Right : Set)  return Boolean;
   function "<=" (Left : Set;  Right : List) return Boolean;
   function "<=" (Left : List; Right : Set)  return Boolean;

   function ">=" (Left : Set;  Right : Set)  return Boolean;
   function ">=" (Left : Set;  Right : List) return Boolean;
   function ">=" (Left : List; Right : Set)  return Boolean;

   -- Membership
   function Member (Element : Element_Type; Of_Set : Set)  return Boolean;
   function Member (Element : Element_Type; Of_Set : List) return Boolean;

   -- Greatest and smallest elements of a set
   function Max (Of_Set : Set) return Element_Type;
   function Min (Of_Set : Set) return Element_Type;
      -- raise SET_EMPTY if the parameter is an empty set.

   -- Cardinality
   function Card (Of_Set  : Set)  return Natural_Count;
   function Card (Of_List : List) return Natural_Count;

   generic
      with procedure Action (On_Element : in Element_Type);
   procedure Enumeration (On_Set : in Set);
      -- Traversal of a set calling ACTION for each element.

   procedure Add (Element  : in Element_Type; To : in out Set);
   procedure Add (Elements : in Set;          To : in out Set);
   procedure Add (Elements : in List;         To : in out Set);

   procedure Remove (Element  : in Element_Type; From : in out Set);
   procedure Remove (Elements : in Set;          From : in out Set);
   procedure Remove (Elements : in List;         From : in out Set);

   procedure Intersect (Left : in Set;  Right : in Set;  To : in out Set);
   procedure Intersect (Left : in Set;  Right : in List; To : in out Set);
   procedure Intersect (Left : in List; Right : in Set;  To : in out Set);
      -- TO := LEFT * RIGHT

   procedure Swap (Left, Right : in out Set);
      -- Exchanges LEFT and RIGHT.

   function To_List (From : Set) return List;
      -- Returns a sorted list of all elements in FROM.

   function Empty (The_Set : Set) return Boolean;

   procedure Assign (Object : in out Set; Value : in Set);
   procedure Assign (Object : in out Set; Value : in List);
      -- OBJECT := VALUE

   procedure Empty (The_Set : in out Set);
      -- Removes all elements from THE_SET.

   Set_Empty : exception;

private

   function Identity (X : Element_Type) return Element_Type;

   pragma Inline(Identity);


   package Element_Bags is new Bags(Key_Type  => Element_Type,
                                    Item_Type => Element_Type,
                                    Key_Of    => Identity,
                                    Count     => Count);

   type Set is new Element_Bags.Bag(Duplicate_Keys_Allowed => False);


   pragma Inline(Min, Max, To_List, Empty);

end General_Sets;
