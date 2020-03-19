-- PROCEDURE TO CHANGE THE CASE OF IDENTIFIERS AND RESERVED WORDS IN ADA SOURCE TEXT
   ---------------------------------------------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Wed Nov  5 13:56:48 1997
-- Update Count    : 12

-- Revision : 19-MAY-1989 by Mats Weber, added treatement of exceptional
--                                       identifiers when capitalized.

-- Creation : 14-NOV-1988 by Mats Weber.


with Text_IO,
     Number_Images,
     Character_Handler,
     String_Handler,
     String_Case_Conversions,
     Ada_Lexical_Analyzer,
     Text_IO_Streams,
     Ada.Strings.Unbounded,
     Bags,
     User_Interface,
     File_Name_Enumeration;

use Text_IO,
    Character_Handler,
    String_Case_Conversions,
    String_Handler,
    Ada_Lexical_Analyzer,
    Ada.Strings.Unbounded;

procedure Makeup_Ada_File is
-------------------------

   type String_Access is access String;

   function Value_Of (A : String_Access) return String;

   function Equal (X, Y : String) return Boolean;

   function Less_Than (X, Y : String) return Boolean;

   package String_Bags is new Bags(Item_Type => String_Access,
                                   Key_Type  => String,
                                   Key_Of    => Value_Of,
                                   "="       => Equal,
                                   "<"       => Less_Than,
                                   Count     => Natural);

   use String_Bags;


   N_Files_Processed,
   N_Files_Modified   : Natural := 0;

   Create_New_Files : constant Boolean :=
                      User_Interface.Yes_No_Answer("Create new versions ? ");

   type Word_Case is (No_Change, Upper_Case, Lower_Case, Capitalized);

   Identifier_Case,
   Reserved_Word_Case    : Word_Case;
   Number_Case           : Word_Case range No_Change..Lower_Case;

   Exceptions            : String_Bags.Bag(Duplicate_Keys_Allowed => False);


   function Value_Of (A : String_Access) return String is
   begin
      return A.all;
   end Value_Of;

   function Equal (X, Y : String) return Boolean is
   begin
      return Upper_Case(X) = Upper_Case(Y);
   end Equal;

   function Less_Than (X, Y : String) return Boolean is
   begin
      return Upper_Case(X) < Upper_Case(Y);
   end Less_Than;


   function Case_Answer (Prompt : String) return Word_Case is
   begin
      loop
         declare

            Answer : constant String := User_Interface.String_Answer(Prompt,
                                                                     Null_String_Allowed   => False,
                                                                     Strip_Leading_Blanks  => True,
                                                                     Strip_Trailing_Blanks => True);

         begin
            if Answer'Length = 1 then
               case Answer(Answer'First) is
                  when 'N' | 'n' =>
                     return No_Change;
                  when 'U' | 'u' =>
                     return Upper_Case;
                  when 'L' | 'l' =>
                     return Lower_Case;
                  when 'C' | 'c' =>
                     return Capitalized;
                  when others =>
                     null;
               end case;
            end if;
         end;
      end loop;
   end Case_Answer;


   procedure Makeup (File_Name : in String) is

      Source_File,
      New_File             : Text_IO.File_Type;

      package Source_Stream is new Text_IO_Streams.Input_Stream(File => Source_File);

      package The_Analyzer is
         new Ada_Lexical_Analyzer.Analyzer(Get                        => Source_Stream.Get,
                                           Include_Separators         => True,
                                           Allow_Alternate_Characters => True);

      Lexel                : Lexical_Element;

      File_Modified,
      Bizarre_Separators,
      Lexical_Errors       : Boolean := False;


      function Recased (Of_Image : String; Casing : Word_Case) return String is
      begin
         case Casing is
            when No_Change =>
               return Of_Image;
            when Upper_Case =>
               return Upper_Case(Of_Image);
            when Lower_Case =>
               return Lower_Case(Of_Image);
            when Capitalized =>
               declare

                  Result          : String(Of_Image'Range);
                  Start           : Positive range Of_Image'Range := Of_Image'First;
                  Next_Underline  : Positive range Of_Image'First..Of_Image'Last + 1;

               begin
                  loop
                     Next_Underline := Locate('_', Within => Of_Image(Start..Of_Image'Last));
                     if Member(Key => Of_Image(Start..Next_Underline - 1), Of_Bag => Exceptions)
                     then
                        Result(Start..Next_Underline - 1) :=
                           Value_Of(Search(Key    => Of_Image(Start..Next_Underline - 1),
                                           Within => Exceptions));
                     else
                        Result(Start..Next_Underline - 1) :=
                           Upper_Case(Of_Image(Start)) &
                           Lower_Case(Of_Image(Start + 1..Next_Underline - 1));
                     end if;
                     exit when Next_Underline > Of_Image'Last;
                     Result(Next_Underline) := '_';
                     Start := Next_Underline + 1;
                  end loop;
                  return Result;
               end;
         end case;
      end Recased;


      procedure Put_To_New_File (Image : in String) is
      begin
         if Create_New_Files then
            Put(New_File, Item => Image);
         end if;
      end Put_To_New_File;

      procedure Put_To_New_File (Image, Old_Image : in String) is
      begin
         if Image /= Old_Image then
            File_Modified := True;
         end if;
         Put_To_New_File(Image);
      end Put_To_New_File;

      procedure New_Line_To_New_File is
      begin
         if Create_New_Files then
            New_Line(New_File);
         end if;
      end New_Line_To_New_File;

      procedure New_Page_To_New_File is
      begin
         if Create_New_Files then
            New_Page(New_File);
         end if;
      end New_Page_To_New_File;


   begin
      Open(Source_File, Name => File_Name, Mode => In_File);
      if Create_New_Files then
         Create(File => New_File, Name => File_Name & ".new");
      end if;
      New_File_Open :
         begin
            loop
               The_Analyzer.Get_Next_Lexical_Element(Lexel);
               case Lexel.Kind is
                  when Separators =>
                     for I in 1..Length(Lexel.Image) loop
                        case Element(Lexel.Image, Index => I) is
                           when ' ' | ASCII.HT | ASCII.VT =>
                              Put_To_New_File((1 => Element(Lexel.Image, Index => I)));
                           when Line_Separator =>
                              New_Line_To_New_File;
                           when Page_Separator =>
                              New_Page_To_New_File;
                           when others =>
                              Bizarre_Separators := True;
                              Put_To_New_File((1 => Element(Lexel.Image, Index => I)));
                        end case;
                     end loop;
                  when Delimiter =>
                     -- replace '!' with '|'
                     if Lexel.The_Delimiter = '|' then
                        Put_To_New_File(Image     => "|",
                                        Old_Image => To_String(Lexel.Image));
                     else
                        Put_To_New_File(To_String(Lexel.Image));
                     end if;
                  when Identifier =>
                     Put_To_New_File(Image     => Recased(To_String(Lexel.Image),
                                                          Casing => Identifier_Case),
                                     Old_Image => To_String(Lexel.Image));
                  when Reserved_Word =>
                     Put_To_New_File(Image     => Recased(To_String(Lexel.Image),
                                                          Casing => Reserved_Word_Case),
                                     Old_Image => To_String(Lexel.Image));
                  when Integer_Literal | Real_Literal =>
                     declare

                        New_Image : String(1..Length(Lexel.Image)) := To_String(Lexel.Image);

                     begin
                        -- replace ':' with '#'
                        for I in New_Image'Range loop
                           if New_Image(I) = ':' then
                              New_Image(I) := '#';
                           end if;
                        end loop;
                        case Number_Case is
                           when No_Change =>
                              null;
                           when Upper_Case =>
                              Upper_Case(New_Image);
                           when Lower_Case =>
                              Lower_Case(New_Image);
                        end case;
                        Put_To_New_File(Image     => New_Image,
                                        Old_Image => To_String(Lexel.Image));
                     end;
                  when String_Literal =>
                     -- replace '%' with '"'
                     Put_To_New_File(Image     => """",
                                     Old_Image => (1 => Element(Lexel.Image, Index => 1)));
                     for I in 1..Length(Lexel.The_String_Literal) loop
                        if Element(Lexel.The_String_Literal, Index => I) = '"' then
                           Put_To_New_File("""""");
                        else
                           Put_To_New_File((1 => Element(Lexel.The_String_Literal, Index => I)));
                        end if;
                     end loop;
                     Put_To_New_File(Image     => """",
                                     Old_Image => (1 => Element(Lexel.Image,
                                                                Index => Length(Lexel.Image))));
                  when Lexical_Error =>
                     Lexical_Errors := True;
                     Put_To_New_File(To_String(Lexel.Image));
                  when End_Of_Source =>
                     exit;
                  when others =>
                     Put_To_New_File(To_String(Lexel.Image));
               end case;
            end loop;
         exception
            when others =>
               if Create_New_Files then
                  Delete(New_File);
               end if;
               raise;
         end New_File_Open;
      if Create_New_Files then
         if File_Modified then
            Close(New_File);
         else
            Delete(New_File);
         end if;
      end if;
      N_Files_Processed := N_Files_Processed + 1;
      if File_Modified then
         N_Files_Modified := N_Files_Modified + 1;
      end if;
      if File_Modified then
         if Create_New_Files then
            Put_Line(File_Name & " modified");
         else
            Put_Line(File_Name & " should be modified");
         end if;
      end if;
      if Bizarre_Separators then
         Put_Line(File_Name & " contains bizarre separators");
      end if;
      if Lexical_Errors then
         Put_Line(File_Name & " has lexical errors");
      end if;
   end Makeup;

   procedure Makeup_All is new File_Name_Enumeration(Makeup);


   function Image is new Number_Images.Integer_Image(Natural);


begin
   Put_Line("Case values :");
   Put_Line("   (N: No changes, U: Upper case, L: Lower case, C: Capitalized)");
   Identifier_Case := Case_Answer("Identifiers      (N, U, L or C) : ");
   if Identifier_Case = Capitalized then
      loop
         declare

            Exceptions_File_Name : constant String :=
                                   User_Interface.String_Answer
                                      ("   Exceptions File Name (<ret> for none) : ");

         begin
            exit when Exceptions_File_Name = "";
            declare

               Exceptions_File : Text_IO.File_Type;

               package Exceptions_Stream is
                  new Text_IO_Streams.Input_Stream(File => Exceptions_File);

               package Exceptions_Analyzer is
                  new Ada_Lexical_Analyzer.Analyzer
                         (Get                        => Exceptions_Stream.Get,
                          Include_Comments           => False,
                          Include_Separators         => False,
                          Allow_Alternate_Characters => False);

               Token : Lexical_Element;

            begin
               Open(Exceptions_File,
                    Name => Exceptions_File_Name,
                    Mode => In_File);
               loop
                  Exceptions_Analyzer.Get_Next_Lexical_Element(Token);
                  case Token.Kind is
                     when Identifier =>
                        Insert(Item => new String'(To_String(Token.Image)),
                               Into => Exceptions);
                     when End_Of_Source =>
                        exit;
                     when others =>
                        Put_Line("      -- Nonidentifier found in exceptions file");
                        if Is_Open(Exceptions_File) then
                           Close(Exceptions_File);
                        end if;
                        Destroy(The_Bag => Exceptions);
                        return;
                  end case;
               end loop;
               exit;
            exception
               when Text_IO.Name_Error =>
                  Put_Line("      -- File not found");
               when String_Bags.Duplicate_Key =>
                  Put_Line("      -- Duplicate exceptional identifier in exceptions file");
                  if Is_Open(Exceptions_File) then
                     Close(Exceptions_File);
                  end if;
                  Destroy(The_Bag => Exceptions);
                  return;
            end;
         end;
      end loop;
   end if;
   Reserved_Word_Case := Case_Answer("Reserved words   (N, U, L or C) : ");
   loop
      begin
         Number_Case := Case_Answer("Numeric literals (N, U or L)    : ");
         exit;
      exception
         when Constraint_Error =>
            null;
      end;
   end loop;
   New_Line;
   Makeup_All(Prompt => "Input File(s) : ");
   New_Line;
   Put_Line(Image(N_Files_Processed) & " file(s) processed");
   if Create_New_Files then
      Put_Line(Image(N_Files_Modified) & " file(s) modified");
   else
      Put_Line(Image(N_Files_Modified) & " file(s) should be modified");
   end if;
end Makeup_Ada_File;
