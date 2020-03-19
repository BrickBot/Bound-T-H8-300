-- TEXT INPUT/OUTPUT FOR VARYING_TEXT
   ----------------------------------

-- Creation : 11-APR-1988 by Mats Weber.

with Varying_Text,
     Text_IO;

use Text_IO;

package Varying_Text_IO is new Varying_Text.Generic_Varying_Text_IO(File_Type => Text_IO.File_Type,
                                                                    Field     => Text_IO.Field,
                                                                    Count     => Text_IO.Count);
