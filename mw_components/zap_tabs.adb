-- PROCEDURE TO REMOVE TABULATORS AND TRAILING BLANKS FROM TEXT FILES
   ------------------------------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Nov 25 14:54:44 1997
-- Update Count    : 4

-- Revision : 31-MAR-1988 by Mats Weber, use library procedure CHECK_FILE_FOR_TABS.
-- Revision : 22-FEB-1988 by Mats Weber, added removal of trailing blanks.

-- Creation :  8-DEC-1987 by Mats Weber.


with Ada.Text_IO,
     Number_Images,
     User_Interface,
     Check_File_For_Tabs,
     File_Name_Enumeration;

use Ada.Text_IO;

procedure Zap_Tabs is
------------------

   N_Files_Processed,
   N_Files_With_Tabs,
   N_Files_With_Trailing_Blanks,
   N_New_Files                    : Natural := 0;


   procedure Zap_Tabs (File_Name : in String) is

      File_Has_Tabs,
      File_Has_Trailing_Blanks : Boolean;

   begin
      Check_File_For_Tabs(File_Name           => File_Name,
                          Create_New_File     => True,
                          Has_Tabs            => File_Has_Tabs,
                          Has_Trailing_Blanks => File_Has_Trailing_Blanks);
      N_Files_Processed := N_Files_Processed + 1;
      if File_Has_Tabs then
         N_Files_With_Tabs := N_Files_With_Tabs + 1;
      end if;
      if File_Has_Trailing_Blanks then
         N_Files_With_Trailing_Blanks := N_Files_With_Trailing_Blanks + 1;
      end if;
      if File_Has_Tabs and File_Has_Trailing_Blanks then
         Put_Line(File_Name &
                  " contained tabs and trailing blanks");
      elsif File_Has_Tabs then
         Put_Line(File_Name &
                  " contained tabs");
      elsif File_Has_Trailing_Blanks then
         Put_Line(File_Name &
                  " contained trailing blanks");
      end if;
      if File_Has_Tabs or File_Has_Trailing_Blanks then
         N_New_Files := N_New_Files + 1;
      end if;
   end Zap_Tabs;

   procedure Zap_All_Tabs is new File_Name_Enumeration(Zap_Tabs);


   function Image is new Number_Images.Integer_Image(Natural);


begin
   Zap_All_Tabs(Prompt => "Input File(s) : ");
   New_Line;
   Put_Line(Image(N_Files_Processed)            & " file(s) processed");
   Put_Line(Image(N_New_Files)                  & " new file(s) created");
   Put_Line(Image(N_Files_With_Tabs)            & " file(s) contained tabs");
   Put_Line(Image(N_Files_With_Trailing_Blanks) & " file(s) contained trailing blanks");
end Zap_Tabs;
