-- TEXT HANDLING PACKAGE
   ---------------------

-- Revision : 11-APR-1988 by Mats Weber, removed dependance on TEXT_IO by creating package
--                                       instance VARYING_TEXT_IO.
-- Revision :  7-APR-1988 by Mats Weber, replaced with an instance of GENERIC_VARYING_TEXT.

-- Creation :  5-JUL-1986 by Mats Weber.


with Generic_Varying_Text,
     String_Handler;

use String_Handler;

package Varying_Text is new Generic_Varying_Text(Character      => Character,
                                                 Null_Character => ASCII.Nul,
                                                 String         => String,
                                                 Side           => String_Handler.Side);
