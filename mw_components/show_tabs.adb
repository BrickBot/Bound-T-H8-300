-- PROCEDURE TO CHECK IF FILES CONTAIN TABULATORS (ASCII.HT) OR TRAILING BLANKS
   ----------------------------------------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Nov 25 14:57:45 1997
-- Update Count    : 3

-- Revision : 31-MAR-1988 by Mats Weber, use library procedure CHECK_FILE_FOR_TABS.

-- Creation :  8-DEC-1987 by Mats Weber.


with Ada.Text_IO,
     Number_Images,
     User_Interface,
     Check_File_For_Tabs,
     File_Name_Enumeration;

use Ada.Text_IO;

procedure Show_Tabs is
-------------------

   N_Files_Processed,
   N_Files_With_Tabs,
   N_Files_With_Trailing_Blanks   : Natural := 0;


   procedure Show_Tabs (File_Name : in String) is

      File_Has_Tabs,
      File_Has_Trailing_Blanks : Boolean;

   begin
      Check_File_For_Tabs(File_Name           => File_Name,
                          Create_New_File     => False,
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
                  " contains tabs and trailing blanks");
      elsif File_Has_Tabs then
         Put_Line(File_Name &
                  " contains tabs");
      elsif File_Has_Trailing_Blanks then
         Put_Line(File_Name &
                  " contains trailing blanks");
      end if;
   end Show_Tabs;

   procedure Show_All_Tabs is new File_Name_Enumeration(Show_Tabs);


   function Image is new Number_Images.Integer_Image(Natural);


begin
   Show_All_Tabs(Prompt => "Input File(s) : ");
   New_Line;
   Put_Line(Image(N_Files_Processed)            & " file(s) processed");
   Put_Line(Image(N_Files_With_Tabs)            & " file(s) contain(s) tabs");
   Put_Line(Image(N_Files_With_Trailing_Blanks) & " file(s) contain(s) trailing blanks");
end Show_Tabs;
