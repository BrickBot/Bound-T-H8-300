-- GENERIC QUICK SORT FUNCTION
   ---------------------------

-- Created :  9-SEP-1986 by Mats Weber.

with Quick_Sort;

function Quick_Sort_Function (V : Vector) return Vector is

  V_Sorted : Vector(V'Range) := V;

  procedure Sort is new Quick_Sort(Index, Item, Vector);

begin
  Sort(V_Sorted);
  return V_Sorted;
end Quick_Sort_Function;
