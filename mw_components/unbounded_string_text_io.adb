-- Filename        : unbounded_string_text_io.adb
-- Description     : IO for Ada.Strings.Unbounded.
-- Author          : Mats Weber
-- Created On      : Tue Feb  3 23:03:10 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Tue Feb  3 23:04:33 1998
-- Update Count    : 1


package body Unbounded_String_Text_IO is
-------------------------------------

   procedure Get_Line (Item : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Item := Get_Line;
   end;

   procedure Get_Line (File : in Ada.Text_IO.File_Type;
                       Item : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Item := Get_Line(File);
   end;

end Unbounded_String_Text_IO;
