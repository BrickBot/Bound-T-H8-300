-- TYPES FOR THE STANDARD INSTANCE OF LARGE_INTEGERS
   -------------------------------------------------

-- Creation : 24-MAY-1988 by Mats Weber.


with Largest_Numeric_Types;

package Large_Integer_Handler_Types is
-----------------------------------

   Slice_Bits : constant := (Largest_Numeric_Types.Large_Integer'Size - 1) / 2;

   type Slice_Value is range 0..2**Slice_Bits - 1;

   type Slice_Arithmetic is range 0..2**(2 * Slice_Bits) - 1;

   type Slice_Index is range 0..10_000;

end Large_Integer_Handler_Types;
