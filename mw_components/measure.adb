-- PROGRAM TO COUNT THE LINES AND SEMICOLONS IN ADA SOURCE TEXT
   ------------------------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Sep  8 12:04:18 1997
-- Update Count    : 2

-- Revision :  6-OCT-1988 by Mats Weber, do not count semicolons in comments,
--                                       string literals and character literals.

-- Creation :  5-SEP-1988 by Mats Weber.


with Text_IO,
     Number_Images,
     User_Interface,
     File_Name_Enumeration;

use Text_IO;

procedure Measure is
-----------------

   Max_Source_Line_Length   : constant := 300;

   Total_Files              : Natural := 0;
   Total_Lines,
   Total_Significant_Lines  : Natural := 0;
   Total_Semicolons         : Natural := 0;
   Total_Characters         : Natural := 0;
   Global_Max_Line_Length   : Natural range 0..Max_Source_Line_Length := 0;


   function Image is new Number_Images.Integer_Image(Natural);


   procedure Put_Numbers (Lines,
                          Significant_Lines : in Natural;
                          Semicolons        : in Natural;
                          Characters        : in Natural;
                          Max_Line_Length   : in Natural) is

      Lines_Output : constant String := Image(Significant_Lines) & "/" & Image(Lines);

   begin
      Put_Line((1..15 - Lines_Output'Length => ' ') & Lines_Output & " lines, " &
               Image(Semicolons, Width => 7) & " ';'s, " &
               Image(Characters, Width => 9) & " characters, " &
               "max line length: " & Image(Max_Line_Length, Width => 3));
   end Put_Numbers;


   procedure Measure (File_Name : in String) is

      Number_Of_Lines,
      Number_Of_Significant_Lines  : Natural := 0;
      Number_Of_Semicolons         : Natural := 0;
      Number_Of_Characters         : Natural := 0;
      Max_Line_Length              : Natural range 0..Max_Source_Line_Length := 0;

      Source_File                  : Text_IO.File_Type;
      Source_Line                  : String(1..Max_Source_Line_Length);
      Source_Line_Length           : Natural range 0..Max_Source_Line_Length;

   begin
      Open(Source_File, Name => File_Name, Mode => In_File);
      while not End_Of_File(Source_File) loop
         Get_Line(Source_File, Item => Source_Line, Last => Source_Line_Length);
         declare

            In_String_Literal      : Boolean := False;
            String_Literal_Opener  : Character;      -- '"' or '%'

            Line_Is_Significant    : Boolean := False;

         begin
            for I in 1..Source_Line_Length loop
               case Source_Line(I) is
                  when '-' =>
                     exit when not In_String_Literal and
                               (I < Source_Line_Length and then Source_Line(I + 1) = '-');   -- Skip comments
                  when '"' | '%' =>
                     if In_String_Literal then
                        if Source_Line(I) = String_Literal_Opener then
                           In_String_Literal := False;
                        end if;
                     elsif I = 1 or else Source_Line(I - 1) /= ''' then
                        String_Literal_Opener := Source_Line(I);
                        In_String_Literal := True;
                     end if;
                  when ';' =>
                     if not In_String_Literal and not (I > 1 and then Source_Line(I - 1) = ''') then
                        Number_Of_Semicolons := Number_Of_Semicolons + 1;
                     end if;
                  when others =>
                     null;
               end case;
               if Source_Line(I) /= ' ' and Source_Line(I) /= ASCII.HT then
                  Line_Is_Significant := True;
               end if;
            end loop;
            if Line_Is_Significant then
               Number_Of_Significant_Lines := Number_Of_Significant_Lines + 1;
            end if;
         end;
         Number_Of_Lines      := Number_Of_Lines + 1;
         Number_Of_Characters := Number_Of_Characters + Source_Line_Length;
         if Source_Line_Length > Max_Line_Length then
            Max_Line_Length := Source_Line_Length;
         end if;
      end loop;
      Close(Source_File);
      Put_Line(File_Name);
      Put_Numbers(Lines             => Number_Of_Lines,
                  Significant_Lines => Number_Of_Significant_Lines,
                  Semicolons        => Number_Of_Semicolons,
                  Characters        => Number_Of_Characters,
                  Max_Line_Length   => Max_Line_Length);
      Total_Files             := Total_Files + 1;
      Total_Lines             := Total_Lines + Number_Of_Lines;
      Total_Significant_Lines := Total_Significant_Lines + Number_Of_Significant_Lines;
      Total_Semicolons        := Total_Semicolons + Number_Of_Semicolons;
      Total_Characters        := Total_Characters + Number_Of_Characters;
      if Max_Line_Length > Global_Max_Line_Length then
         Global_Max_Line_Length := Max_Line_Length;
      end if;
   end Measure;

   procedure Measure_All is new File_Name_Enumeration(Measure);

begin
   Measure_All(Prompt => "File(s) to measure : ");
   New_Line;
   Put_Line("Total of " & Image(Total_Files) & " file(s)");
   Put_Numbers(Lines             => Total_Lines,
               Significant_Lines => Total_Significant_Lines,
               Semicolons        => Total_Semicolons,
               Characters        => Total_Characters,
               Max_Line_Length   => Global_Max_Line_Length);
end Measure;
