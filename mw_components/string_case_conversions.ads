-- STRING CASE CONVERSIONS
   -----------------------

-- Creation : 17-NOV-1989 by Mats Weber.


with Case_Conversions,
     Character_Handler;

package String_Case_Conversions is
-------------------------------
   new Case_Conversions(Character,
                        String,
                        Upper_Case => Character_Handler.Upper_Case,
                        Lower_Case => Character_Handler.Lower_Case);
