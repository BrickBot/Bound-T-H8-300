-- PROCEDURE TO CHECK IF TEXT FILES CONTAIN TABS OR TRAILING BLANKS AND REMOVE THEM
   --------------------------------------------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Nov 25 15:25:56 1997
-- Update Count    : 3

-- Revision : 30-MAR-1990 by Mats Weber, use 8 bit characters (with Local_Character_Set).

-- Creation : 31-MAR-1988 by Mats Weber.


with Ada.Text_IO,
     String_Handler,
     String_Text_IO;

use String_Handler;

procedure Check_File_For_Tabs (File_Name           : in String;
                               Create_New_File     : in Boolean;
                               Has_Tabs,
                               Has_Trailing_Blanks : out Boolean) is

   -- Checks if the file contains tabs or trailing blanks.
   -- If CREATE_NEW_FILE is TRUE and the file contains tabs or trailing blanks,
   -- a new clean version of the file will be created.


   Tab_Width : constant := 8;

   Input_File                    : Ada.Text_IO.File_Type;
   File_Contains_Tabs,
   File_Contains_Trailing_Blanks : Boolean := False;


   function Last_Valid_Char_Pos (Within : String) return Natural is

      function Non_Blank (Ch : Character) return Boolean is
      begin
         return Ch /= ' ' and Ch /= ASCII.HT;
      end;

      function Search_For_Non_Blank is new String_Handler.Scan(Non_Blank);

   begin
      return Search_For_Non_Blank(Within, From => Right);
   end Last_Valid_Char_Pos;


begin
   Ada.Text_IO.Open(File => Input_File,
                    Name => File_Name,
                    Mode => Ada.Text_IO.In_File);
   while not Ada.Text_IO.End_Of_File(Input_File) loop
      declare

         Current_Line : constant String := String_Text_IO.Get_Line(Input_File);

      begin
         File_Contains_Tabs := File_Contains_Tabs or
                               Located(ASCII.HT, Within => Current_Line);
         File_Contains_Trailing_Blanks := File_Contains_Trailing_Blanks or
                                          Last_Valid_Char_Pos(Current_Line) < Current_Line'Last;
      end;
      exit when File_Contains_Tabs and File_Contains_Trailing_Blanks;
   end loop;

   if Create_New_File and (File_Contains_Tabs or File_Contains_Trailing_Blanks) then
      Ada.Text_IO.Reset(Input_File);
      declare

         Output_File : Ada.Text_IO.File_Type;

      begin
         Ada.Text_IO.Create(File => Output_File,
                            Name => File_Name & ".new");
         while not Ada.Text_IO.End_Of_File(Input_File) loop
            while not Ada.Text_IO.End_Of_Page(Input_File) loop
               declare

                  Current_Line : constant String := String_Text_IO.Get_Line(Input_File);
                  Ch           : Character;
                  Col_Ch       : Ada.Text_IO.Count range 0 .. Ada.Text_IO.Count'Last := 0;
                  New_Col_Ch   : Ada.Text_IO.Count range 0 .. Ada.Text_IO.Count'Last;

                  use type Ada.Text_IO.Count;

               begin
                  for I in Current_Line'First .. Last_Valid_Char_Pos(Current_Line) loop
                     Ch := Current_Line(I);
                     if Ch = ASCII.HT then
                        New_Col_Ch := (Col_Ch / Tab_Width + 1) * Tab_Width;
                        Ada.Text_IO.Put(Output_File, (1 .. Integer(New_Col_Ch - Col_Ch) => ' '));
                        Col_Ch := New_Col_Ch;
                     else
                        Col_Ch := Col_Ch + 1;
                        Ada.Text_IO.Put(Output_File, Ch);
                     end if;
                  end loop;
               end;
               Ada.Text_IO.New_Line(Output_File);
            end loop;
            if not Ada.Text_IO.End_Of_File(Input_File) then
               Ada.Text_IO.Skip_Page(Input_File);
               Ada.Text_IO.New_Page(Output_File);
            end if;
         end loop;
         Ada.Text_IO.Close(Output_File);
      end;
   end if;
   Ada.Text_IO.Close(Input_File);
   Has_Tabs            := File_Contains_Tabs;
   Has_Trailing_Blanks := File_Contains_Trailing_Blanks;
end Check_File_For_Tabs;
