-- GENERIC PACKAGE FOR DYNAMIC ARRAYS
   ----------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Jun  8 11:34:56 1998
-- Update Count    : 4

-- Revision : 31-AUG-1988 by Mats Weber, added procedure SWAP.
-- Revision : 27-JUN-1988 by Mats Weber, renamed type D_ARRAY to DYNAMIC_ARRAY.
-- Revision : 21-APR-1988 by Mats Weber, added functions FIRST and LAST.
-- Revision :  6-NOV-1987 by Mats Weber, changed parameter names for clearer calls.
-- Revision : 25-SEP-1986 by Mats Weber, added generic procedure TRAVERSAL.
-- Revision : 23-SEP-1986 by Mats Weber, added procedure DELETE.
-- Revision :  9-SEP-1986 by Mats Weber, added function PRESENT and
--                                       renamed SET to ASSIGN.

-- Creation : 10-FEB-1986 by Mats Weber


with Tables;

generic
   type Index_Type is private;
   type Component_Type is private;
   with function "<" (I1, I2 : Index_Type) return Boolean is <>;
   type Count is range <>;   -- must include 0
package Dynamic_Arrays is
----------------------

   type Dynamic_Array is limited private;
   ------------------

   Nonexistent_Index,
   Dynamic_Array_Empty : exception;


   subtype Natural_Count is Count range 0..Count'Last;


   procedure Assign (Object : in out Dynamic_Array;
                     Index  : in Index_Type;
                     Value  : in Component_Type);
      -- OBJECT(INDEX) := VALUE;

   function Component (From : Dynamic_Array; Index : Index_Type) return Component_Type;
      -- Returns component INDEX.
      -- Raises NONEXISTENT_INDEX if there is no entry for INDEX in FROM.

   function Present (Within : Dynamic_Array; Index : Index_Type) return Boolean;
      -- Checks if INDEX is present in WITHIN.

   function First (Of_Array : Dynamic_Array) return Index_Type;
   function Last  (Of_Array : Dynamic_Array) return Index_Type;
      -- Returns the smallest (greatest) index in OF_ARRAY.
      -- Will raise DYNAMIC_ARRAY_EMPTY if OF_ARRAY is empty.

   procedure Remove (From : in out Dynamic_Array; Index : in Index_Type);
      -- Removes index INDEX from FROM.
      -- No exception is raised if there is no entry for INDEX in FROM.

   procedure Remove (From      : in out Dynamic_Array;
                     Index     : in Index_Type;
                     Component : out Component_Type);
      -- Removes index INDEX from FROM. COMPONENT returns the component at INDEX.
      -- Raises NONEXISTENT_INDEX if there is no entry for INDEX in FROM.

   generic
      with procedure Action (Index : in Index_Type; Component : in Component_Type);
   procedure Traversal (On_Array : in Dynamic_Array);
      -- Enumerates all index entries in ON_ARRAY.

   generic
      with procedure Update (Index : in Index_Type; Component : in out Component_Type);
   procedure Update_All (Within : in out Dynamic_Array);
      -- Enumerates all index entries in WITHIN,
      -- allowing to modify the components.

   procedure Assign (Object : in out Dynamic_Array; Value : in Dynamic_Array);
      -- OBJECT := VALUE;

   function Card (Of_Array : Dynamic_Array) return Natural_Count;
      -- Returns the number of entries in OF_ARRAY.

   function Empty (The_Array : Dynamic_Array) return Boolean;
      -- Checks if THE_ARRAY is empty.

   procedure Destroy (The_Array : in out Dynamic_Array);
      -- Removes all indices and components from THE_ARRAY.

   procedure Swap (Left, Right : in out Dynamic_Array);
      -- Exchanges LEFT and RIGHT.

private

   function "=" (Left, Right : Component_Type) return Boolean is abstract;
      -- Make sure we don't use equality on Component_Type, we want to use
      -- it only on Index_Type.

   package I_C_Tables is new Tables(Key_Type  => Index_Type,
                                    Item_Type => Component_Type,
                                    Count     => Count);

   type Dynamic_Array is new I_C_Tables.Table;


   pragma Inline(Assign, Component, Present, First, Last,
                 Remove, Card, Empty, Destroy, Swap);

end Dynamic_Arrays;
