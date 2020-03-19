-- FILTERS FOR ACCESSING TEXT_IO FILES AS A STREAM OF CHARACTERS
   -------------------------------------------------------------

-- Creation : 25-SEP-1989 by Mats Weber.


package body Text_IO_Streams is
----------------------------

   use Text_IO;


   package body Input_Stream is
   -------------------------

      procedure Get (Item : out Character) is
      begin
         if End_Of_Line(File) then
            if End_Of_Page(File) then
               if End_Of_File(File) then
                  Item := File_Terminator;
                  End_Action(File);
               else
                  Item := Page_Separator;
                  Skip_Page(File);
               end if;
            else
               Item := Line_Separator;
               Skip_Line(File);
            end if;
         else
            Get(File, Item);
         end if;
      end Get;

   end Input_Stream;


   package body Output_Stream is
   --------------------------

      procedure Put (Item : in Character) is
      begin
         case Item is
            when Line_Separator =>
               New_Line(File);
            when Page_Separator =>
               New_Page(File);
            when File_Terminator =>
               End_Action(File);
            when others =>
               Put(File, Item);
         end case;
      end Put;

   end Output_Stream;

end Text_IO_Streams;
