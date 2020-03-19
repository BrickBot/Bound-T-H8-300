-- Enumerate file names in a portable way
   --------------------------------------

-- Filename        : file_name_enumeration-unix.a
-- Author          : Mats Weber
-- Created On      : Wed Feb 12 19:01:12 1997
-- Last Modified By: Mats Weber
-- Last Modified On: Mon Sep  8 11:51:13 1997
-- Update Count    : 7


with Ada.Command_Line;

procedure File_Name_Enumeration (Prompt : in String) is
-------------------------------

   -- Prompt is not used: the file names are taken from the command line.

begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      Process(Ada.Command_Line.Argument(I));
   end loop;
end File_Name_Enumeration;
