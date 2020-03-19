-- DEFINITIONS FOR TEXT STREAMS
   ----------------------------

-- Creation : 25-SEP-1989 by Mats Weber.


package Text_Stream_Definitions is
-------------------------------

   Line_Separator   : constant Character := ASCII.LF;
   Page_Separator   : constant Character := ASCII.FF;
   File_Terminator  : constant Character := ASCII.EOT;

      -- A Page_Separator means a line terminator followed by a page terminator.
      -- A File_Terminator means a line terminator followed by a page terminator
      -- followed by a file terminator.

end Text_Stream_Definitions;
