-- FILTERS FOR ACCESSING TEXT_IO FILES AS A STREAM OF CHARACTERS
   -------------------------------------------------------------

-- Creation : 25-SEP-1989 by Mats Weber.


with Text_Stream_Definitions,
     Text_IO;

package Text_IO_Streams is
-----------------------

   Line_Separator   : Character renames Text_Stream_Definitions.Line_Separator;
   Page_Separator   : Character renames Text_Stream_Definitions.Page_Separator;
   File_Terminator  : Character renames Text_Stream_Definitions.File_Terminator;


   generic
      File : in out Text_IO.File_Type;
      with procedure End_Action (File : in out Text_IO.File_Type) is
         Text_IO.Close;
   package Input_Stream is

      procedure Get (Item : out Character);
         -- Returns all characters in File in sequence.
         -- Line_Separator is returned to separate lines,
         -- Page_Separator is returned to separate pages
         --    (without any preceeding Line_Separator, except for blank lines),
         -- File_Terminator is returned at the end of File
         --    (without any preceeding Line_Separator or Page_Separator,
         --     except for blank lines or pages)
         --    and End_Action(File) is called.

      pragma Inline(Get);

   end;


   generic
      File : in out Text_IO.File_Type;
      with procedure End_Action (File : in out Text_IO.File_Type) is
         Text_IO.Close;
   package Output_Stream is

      procedure Put (Item : in Character);
         -- Does the inverses of Input_Stream.Get.
         -- End_Action(File) is called when Item = File_Terminator.

      pragma Inline(Put);

   end;

end Text_IO_Streams;
