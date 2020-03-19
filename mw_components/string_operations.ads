-- GENERIC PACKAGE FOR STRING HANDLING
   -----------------------------------

-- Revision : 16-NOV-1989 by Mats Weber, removed Upper_Case and Lower_Case,
--                                       added Transform and Transformation_Function.
-- Revision : 21-FEB-1989 by Mats Weber, made generic formal type CHARACTER private instead of (<>).
-- Revision :  7-APR-1988 by Mats Weber, renamed to STRING_OPERATIONS and made generic.
-- Revision : 11-MAR-1988 by Mats Weber, added function STRING_1.
-- Revision : 10-DEC-1987 by Mats Weber, moved generic function INTEGER_IMAGE to package NUMBER_IMAGES.
-- Revision :  9-DEC-1987 by Mats Weber, added function LOCATED.
-- Revision : 20-JAN-1987 by Mats Weber, added generic function INTEGER_IMAGE
--                                       to avoid the leading space in INTEGER'IMAGE.
-- Revision : 31-JUL-1986 by Mats Weber, added generic function SCAN.
-- Creation : 29-JUN-1986 by Mats Weber.


generic
   type Character is private;
   type String is array (Positive range <>) of Character;
package String_Operations is
-------------------------

   type Side is (Left, Right);

   function String_1 (Of_String : String) return String;
      -- Returns Of_String with bounds 1..Of_String'Length.

   generic
      with function Condition (The_Character : Character) return Boolean;
   function Scan (Within : String;
                  From   : Side := Left) return Natural;
      -- Returns the position of the first character for which Condition
      -- returns True.

   generic
      with procedure Transform (The_Character : in out Character);
   procedure Transformation (On_String : in out String);
      -- Modifies On_String by calling Transform for Each of its
      -- characters.

   generic
      with function Transformed (The_Character : Character) return Character;
   function Transformation_Function (Of_String : String) return String;
      -- Returns Of_String with each character Transformed.


   function Locate (Pattern : Character;
                    Within  : String;
                    From    : Side := Left) return Natural;

   function Locate (Pattern : String;
                    Within  : String;
                    From    : Side := Left) return Natural;
      -- Returns the position of the first character of the first occurence
      -- of Pattern within Within. If Pattern is not found, then Within'First-1
      -- is returned when From=Right, or Within'Last+1 when From=Left.
      -- From indicates where the search must begin.

   function Located (Pattern : Character; Within : String) return Boolean;
   function Located (Pattern : String;    Within : String) return Boolean;
      -- Returns True if and only if Pattern is a substring of Within.

   function Delete (Pattern : Character;
                    Within  : String;
                    From    : Side := Left) return String;

   function Delete (Pattern : String;
                    Within  : String;
                    From    : Side := Left) return String;
      -- Returns Within with the first occurence of Pattern removed.

   function "-" (Left : String; Right : Character) return String;
   function "-" (Left : String; Right : String)    return String;
      -- Returns Left with all occurences of Right removed.


   pragma Inline(Scan, Transformation, Transformation_Function);

end String_Operations;
