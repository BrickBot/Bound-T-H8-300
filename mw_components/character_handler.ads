-- PACKAGE FOR CHARACTER HANDLING
   ------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon May  4 19:12:03 1998
-- Update Count    : 5

-- Revision :  3-Feb-1998 by Mats Weber, used Ada 95 standard functions in order to be
--                                       compatible with ISO-Latin-1
-- Revision : 16-MAY-1988 by Mats Weber, removed subtypes CONTROL_CHARACTER, etc. and
--                                       added functions IS_CONTROL, etc.

-- Creation :  7-APR-1988 by Mats Weber.


with Ada.Characters.Handling;

package Character_Handler is
-------------------------

   function Is_Control    (The_Character : Character) return Boolean
      renames Ada.Characters.Handling.Is_Control;
   function Is_Graphic    (The_Character : Character) return Boolean
      renames Ada.Characters.Handling.Is_Graphic;
   function Is_Upper_Case (The_Character : Character) return Boolean
      renames Ada.Characters.Handling.Is_Upper;
   function Is_Lower_Case (The_Character : Character) return Boolean
      renames Ada.Characters.Handling.Is_Lower;
   function Is_Digit      (The_Character : Character) return Boolean
      renames Ada.Characters.Handling.Is_Digit;
   function Is_Letter     (The_Character : Character) return Boolean
      renames Ada.Characters.Handling.Is_Letter;
   function Is_Symbol     (The_Character : Character) return Boolean
      renames Ada.Characters.Handling.Is_Special;


   -- Conversion to upper case
   function Upper_Case (Of_Character : Character) return Character
      renames Ada.Characters.Handling.To_Upper;

   procedure Upper_Case (The_Character : in out Character);

   -- Conversion to lower case
   function Lower_Case (Of_Character : Character) return Character
      renames Ada.Characters.Handling.To_Lower;

   procedure Lower_Case (The_Character : in out Character);


   -- Digit handling
   subtype Digit_Number is Natural range 0 .. 15;

   function Digit_Image (Of_Number : Digit_Number) return Character;
   function Digit_Value (Of_Character : Character) return Digit_Number;
      -- Will raise NOT_A_DIGIT if OF_CHARACTER is
      -- not in '0' .. '9' or 'A' .. 'F' or 'a' .. 'f'.

   Not_A_Digit : exception;


   pragma Inline(Upper_Case, Lower_Case);

end Character_Handler;
