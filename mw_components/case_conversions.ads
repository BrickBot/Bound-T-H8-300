-- GENERIC STRING CASE CONVERSIONS
   -------------------------------

-- Creation : 17-NOV-1989 by Mats Weber.


generic
   type Character is private;
   type String is array (Positive range <>) of Character;
   with function Upper_Case (The_Character : Character) return Character is <>;
   with function Lower_Case (The_Character : Character) return Character is <>;
package Case_Conversions is
------------------------

   procedure Upper_Case (The_String : in out String);
   procedure Lower_Case (The_String : in out String);

   function Upper_Case (Of_String : String) return String;
   function Lower_Case (Of_String : String) return String;

end Case_Conversions;
