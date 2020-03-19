-- Enumerate file names in a portable way
   --------------------------------------

-- Filename        : file_name_enumeration-vms.a
-- Author          : Mats Weber
-- Created On      : Wed Feb 12 19:01:12 1997
-- Last Modified By: Mats Weber
-- Last Modified On: Wed Feb 12 19:07:52 1997
-- Update Count    : 3


with VMS_File_Names,
     User_Interface;

procedure File_Name_Enumeration (Prompt : in String) is
-------------------------------

   procedure Process_All is new VMS_File_Names.Enumerate_Matching_Files(Action => Process);

begin
   Process_All(File_Name => User_Interface.String_Answer(Prompt));
end File_Name_Enumeration;
