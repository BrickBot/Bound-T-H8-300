-- Filename        : string_text_io.ads
-- Description     : IO for strings
-- Author          : Mats Weber
-- Created On      : Tue Nov 25 14:22:22 1997
-- Last Modified By: Mats Weber
-- Last Modified On: Tue Nov 25 14:31:09 1997
-- Update Count    : 2


with Ada.Text_IO,
     GNAT.IO_Aux;

package String_Text_IO is
----------------------

   function Get_Line return String renames GNAT.IO_Aux.Get_Line;

   function Get_Line (File : Ada.Text_IO.File_Type) return String renames GNAT.IO_Aux.Get_Line;

end String_Text_IO;
