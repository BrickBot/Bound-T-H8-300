-- Filename        : unbounded_string_text_io.ads
-- Description     : IO for Ada.Strings.Unbounded.
-- Author          : Mats Weber
-- Created On      : Wed Nov  5 14:19:48 1997
-- Last Modified By: Mats Weber
-- Last Modified On: Tue Feb  3 23:02:32 1998
-- Update Count    : 7


with Ada.Strings.Unbounded,
     Ada.Text_IO,
     Ada.Strings.Unbounded.Text_IO;  -- GNAT specific.

package Unbounded_String_Text_IO is
--------------------------------

   function Get_Line return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.Text_IO.Get_Line;
   function Get_Line (File : Ada.Text_IO.File_Type) return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.Text_IO.Get_Line;

   procedure Get_Line (Item : out Ada.Strings.Unbounded.Unbounded_String);
   procedure Get_Line (File : in Ada.Text_IO.File_Type;
                       Item : out Ada.Strings.Unbounded.Unbounded_String);

   procedure Put (Item : Ada.Strings.Unbounded.Unbounded_String)
      renames Ada.Strings.Unbounded.Text_IO.Put;
   procedure Put (File : Ada.Text_IO.File_Type; Item : Ada.Strings.Unbounded.Unbounded_String)
      renames Ada.Strings.Unbounded.Text_IO.Put;

   procedure Put_Line (Item : Ada.Strings.Unbounded.Unbounded_String)
      renames Ada.Strings.Unbounded.Text_IO.Put_Line;
   procedure Put_Line (File : Ada.Text_IO.File_Type; Item : Ada.Strings.Unbounded.Unbounded_String)
      renames Ada.Strings.Unbounded.Text_IO.Put_Line;

end Unbounded_String_Text_IO;
