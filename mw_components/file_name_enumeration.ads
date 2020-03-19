-- Enumerate file names in a portable way
   --------------------------------------

-- Filename        : file_name_enumeration_.a
-- Author          : Mats Weber
-- Created On      : Wed Feb 12 18:58:39 1997
-- Last Modified By: Mats Weber
-- Last Modified On: Wed Feb 12 19:09:49 1997
-- Update Count    : 3


generic
   with procedure Process (File_Name : in String);
procedure File_Name_Enumeration (Prompt : in String);
-------------------------------
