-- PACKAGE DEFINING THE LARGEST POSSIBLE NUMERIC TYPES FOR ANY IMPLEMENTATION
   --------------------------------------------------------------------------

-- Creation : 27-MAY-1988 by Mats Weber.


with System;

package Largest_Numeric_Types is
-----------------------------

   type Large_Integer is range System.Min_Int..System.Max_Int;

   type Large_Float is digits System.Max_Digits;

   type Fine_Fixed is delta System.Fine_Delta range -1.0..+1.0;

end Largest_Numeric_Types;
