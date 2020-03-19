-- STANDARD INSTANCE OF LARGE_INTEGERS
   -----------------------------------

-- Creation : 24-MAY-1988 by Mats Weber.


with Large_Integers,
     Large_Integer_Handler_Types;

package Large_Integer_Handler is
   new Large_Integers(Slice_Number      => Large_Integer_Handler_Types.Slice_Value,
                      Arithmetic_Number => Large_Integer_Handler_Types.Slice_Arithmetic,
                      Slice_Index       => Large_Integer_Handler_Types.Slice_Index);
