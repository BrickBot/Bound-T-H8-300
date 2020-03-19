-- GENERIC PACKAGE FOR SETS OF DISCRETE TYPES
   ------------------------------------------

-- Revision : 31-AUG-1988 by Mats Weber, added procedure SWAP.
-- Revision : 28-JUN-1988 by Mats Weber, renamed package from SETS to DISCRETE_SETS.
-- Revision :  7-DEC-1987 by Mats Weber, added function MEMBER(ELEMENT_TYPE, LIST).
-- Revision :  4-NOV-1987 by Mats Weber, added generic formal type COUNT.
-- Revision : 11-DEC-1985 by Mats Weber.

-- Creation : 20-JUN-1985 by S. Mourtada.


generic
  type Element_Type is (<>);
  type Count is range <>;   -- must include 0
package Discrete_Sets is
---------------------

  type Set is private;
  --------

  subtype Natural_Count  is Count range 0..Count'Last;
  subtype Positive_Count is Count range 1..Count'Last;

  type List is array (Positive_Count range <>) of Element_Type;

  Full_Set, Empty_Set : constant Set;


  -- Set Constructors
  function New_Set (Element  : Element_Type) return Set;
  function New_Set (Elements : List)         return Set;

  -- Complement (unary "-")
  function "-" (Right : Set) return Set;

  -- Union
  function "+" (Left : Set;          Right : Set)          return Set;
  function "+" (Left : Set;          Right : List)         return Set;
  function "+" (Left : List;         Right : Set)          return Set;
  function "+" (Left : Set;          Right : Element_Type) return Set;
  function "+" (Left : Element_Type; Right : Set)          return Set;

  -- Intersection
  function "*" (Left : Set;  Right : Set)  return Set;
  function "*" (Left : Set;  Right : List) return Set;
  function "*" (Left : List; Right : Set)  return Set;

  -- Difference
  function "-" (Left : Set;  Right : Set)          return Set;
  function "-" (Left : Set;  Right : List)         return Set;
  function "-" (Left : List; Right : Set)          return Set;
  function "-" (Left : Set;  Right : Element_Type) return Set;

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

  -- Traversal
  generic
    with procedure Action (On_Element : in Element_Type);
  procedure Enumeration (On_Set : in Set);
    -- Traversal of a set calling ACTION for each element

  procedure Add (Element  : in Element_Type; To : in out Set);
  procedure Add (Elements : in Set;          To : in out Set);
  procedure Add (Elements : in List;         To : in out Set);

  procedure Remove (Element  : in Element_Type; From : in out Set);
  procedure Remove (Elements : in Set;          From : in out Set);
  procedure Remove (Elements : in List;         From : in out Set);

  procedure Intersect (Left : in Set;  Right : in Set;  To : out Set);
  procedure Intersect (Left : in Set;  Right : in List; To : out Set);
  procedure Intersect (Left : in List; Right : in Set;  To : out Set);
    -- TO := LEFT * RIGHT

  procedure Swap (Left, Right : in out Set);
    -- Exchanges LEFT and RIGHT.

  function To_List (From : Set) return List;
    -- Returns a sorted list of all elements in FROM.

  function Full  (The_Set : Set) return Boolean;
  function Empty (The_Set : Set) return Boolean;

  procedure Empty (The_Set : out Set);
    -- Removes all elements from THE_SET.

  Set_Empty : exception;

private

  type Set is array (Element_Type) of Boolean;

  pragma Pack(Set);

  Full_Set  : constant Set := (others => True);
  Empty_Set : constant Set := (others => False);


  pragma Inline("+", "*", "-", "<", ">", "<=", ">=", Intersect, Full, Empty);

end Discrete_Sets;
