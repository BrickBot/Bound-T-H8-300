-- VMS FILE NAME HANDLER
   ---------------------

-- Revision : 18-Jun-1993 by Mats Weber, removed function RMS_Condition and
--                                       associated global variable.
-- Revision :  1-MAY-1989 by Mats Weber, added parameter ALTER_PASSWORD to functions FULL_FILE_NAME
--                                       and procedures ENUMERATE_MATCHING_FILES and
--                                       ENUMERATE_MATCHING_FILE_SPECIFICATIONS.
-- Revision :  7-DEC-1987 by Mats Weber, added function FILE_NAME_STRING.
-- Revision :  4-DEC-1987 by Mats Weber, added generic procedure ENUMERATE_MATCHING_FILE_SPECIFICATIONS.
-- Revision : 31-MAR-1987 by Mats Weber, use of package VARYING_TEXT
--                                       and added DEFAULT to ENUMERATE_MATCHING_FILES.

-- Creation : 20-SEP-1986 by Mats Weber.


with Varying_Text,
     Starlet;

use Varying_Text;

package VMS_File_Names is
----------------------

  Max_File_Spec_Length   : constant := Starlet.Nam_C_MaxRSS;

  Max_Node_Name_Length   : constant := Max_File_Spec_Length;
  Max_Device_Length      : constant := Max_File_Spec_Length;
  Max_Directory_Length   : constant := Max_File_Spec_Length;
  Max_File_Name_Length   : constant := 39;
  Max_File_Type_Length   : constant := 39;
  Max_Version_Length     : constant := 5;


  type File_Name_Fields is
    record
      Node_Name   : Text(Max_Node_Name_Length);        -- includes "::"
      Device      : Text(Max_Device_Length);           -- includes ":"
      Directory   : Text(Max_Directory_Length);
      File_Name   : Text(Max_File_Name_Length);
      File_Type   : Text(Max_File_Type_Length + 1);    -- includes "."
      Version     : Text(Max_Version_Length + 1);      -- includes ";"
    end record;

  -- In the following functions and procedures, if the parameter
  -- ALTER_PASSWORD is false, then the file names returned (or passed
  -- to the ACTION procedure) will have the password unaltered if the
  -- node name contains a password. If ALTER_PASSWORD is true, the
  -- password will be replaced with the string "password", which is
  -- useful if the file specification must be displayed.


  function File_Name_String (File_Specification : File_Name_Fields) return String;

  function Full_File_Name (File_Name      : String;
                           Default        : String := "";
                           Alter_Password : Boolean := False) return String;

  function Full_File_Name (File_Name      : String;
                           Default        : String := "";
                           Alter_Password : Boolean := False) return File_Name_Fields;

  function Local_Node_Name return String;

  generic
    with procedure Action (File_Name : in String);
  procedure Enumerate_Matching_Files (File_Name      : in String;
                                      Default        : in String := "";
                                      Alter_Password : in Boolean := False);

  generic
    with procedure Action (File_Specification : in File_Name_Fields);
  procedure Enumerate_Matching_File_Specifications (File_Name      : in String;
                                                    Default        : in String := "";
                                                    Alter_Password : in Boolean := False);


  File_Error : exception;


  -- IO_EXCEPTIONS.NAME_ERROR will be raised when there is an error in
  -- the syntax of a file name.
  -- FILE_ERROR will be raised when the file system detects an error
  -- but the syntax of all parameters is correct
  -- (for example directory not found).

end VMS_File_Names;
