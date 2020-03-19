-- PACKAGE FOR STRING HANDLING
   ---------------------------

-- Revision :  7-APR-1988 by Mats Weber, replace with an instance of
--                                       generic package STRING_OPERATIONS.
-- Creation :  5-JUL-1986 by Mats Weber.


with String_Operations;

package String_Handler is
----------------------
   new String_Operations(Character => Character,
                         String    => String);
