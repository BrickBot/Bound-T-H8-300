-- PROGRAM TO CONVERT ADA SOURCE INTO RTF FORMAT WITH RESERVED WORDS IN BOLD
   -------------------------------------------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Thu Jan 22 17:50:02 1998
-- Update Count    : 10

-- Creation : 11-NOV-1990 by Mats Weber.


with Ada_Lexical_Analyzer,
     Number_Images,
     Text_IO,
     Text_IO_Streams,
     String_Handler,
     Ada.Strings.Unbounded,
     User_Interface,
     File_Name_Enumeration;

use Ada_Lexical_Analyzer,
    String_Handler,
    Ada.Strings.Unbounded,
    Text_IO;

procedure RTF_Ada_File is
----------------------

   function Natural_Answer is new User_Interface.Integer_Answer(Natural);


   N_Files_Processed : Natural := 0;

   Italicize_Comments : constant Boolean :=
      User_Interface.Yes_No_Answer("Italicize Comments ? ");

   Number_Of_Line_Breaks_Per_Paragraph : constant Natural :=
      Natural_Answer
         ("Number of line breaks per paragraph break (0 => no paragraphs) ? ");


   procedure RTF_Ada_File (Source_Name : in String) is

      Source,
      Destination : Text_IO.File_Type;

      package Source_Stream is new Text_IO_Streams.Input_Stream(File => Source);

      package The_Analyzer is
         new Ada_Lexical_Analyzer.Analyzer(Get                        => Source_Stream.Get,
                                           Include_Separators         => True,
                                           Allow_Alternate_Characters => True);

      Lex                        : Lexical_Element;
      N_Pending_New_Lines        : Natural range 0 .. Number_Of_Line_Breaks_Per_Paragraph := 0;

      Document_Start_Sequence    : constant String := "{\rtf ";
      Normal_Sequence            : constant String := "}";
      Bold_Sequence              : constant String := "{\b ";
      Italic_Sequence            : constant String := "{\i ";
      New_Line_Sequence          : constant String := "\line";
      New_Paragraph_Sequence     : constant String := "\par";
      New_Page_Sequence          : constant String := "\page";
      Document_End_Sequence      : constant String := "}";


      function Image is new Number_Images.Integer_Image(Text_IO.Count);


      function Double (The_Char : Character; In_String : String) return String is

         Result : Ada.Strings.Unbounded.Unbounded_String;

      begin
         for I in In_String'Range loop
            Append(Result, New_Item => In_String(I));
            Result := Result & In_String(I);
            if In_String(I) = The_Char then
               Append(Result, New_Item => In_String(I));
            end if;
         end loop;
         return To_String(Result);
      end Double;


      procedure Output_Pending_New_Lines is
      begin
         for K in 1..N_Pending_New_Lines loop
            Put(Destination, New_Line_Sequence);
            New_Line(Destination);
         end loop;
         N_Pending_New_Lines := 0;
      end Output_Pending_New_Lines;


   begin
      Put_Line(Source_Name);
      Open(Source, Name => Source_Name, Mode => In_File);
      Create(File => Destination,
             Name => Source_Name(Source_Name'First ..
                                 Locate(".a", Source_Name, From => Right) - 1) &
                     ".rtf");
      Put(Destination, Document_Start_Sequence);
      loop
         The_Analyzer.Get_Next_Lexical_Element(Lex);
         if Lex.Kind /= Separators and N_Pending_New_Lines > 0 then
            Output_Pending_New_Lines;
         end if;
         case Lex.Kind is
            when Separators =>
               for I in 1..Length(Lex.Image) loop
                  if Element(Lex.Image, Index => I) /= Line_Separator and
                     N_Pending_New_Lines > 0
                  then
                     Output_Pending_New_Lines;
                     N_Pending_New_Lines := 0;
                  end if;
                  case Element(Lex.Image, Index => I) is
                     when ' ' | ASCII.HT | ASCII.VT =>
                        Put(Destination, Element(Lex.Image, Index => I));
                     when Line_Separator =>
                        if Number_Of_Line_Breaks_Per_Paragraph = 0 then
                           Put(Destination, New_Line_Sequence);
                           New_Line(Destination);
                        else
                           N_Pending_New_Lines := N_Pending_New_Lines + 1;
                           if N_Pending_New_Lines = Number_Of_Line_Breaks_Per_Paragraph then
                              Put(Destination, New_Paragraph_Sequence);
                              New_Line(Destination);
                              N_Pending_New_Lines := 0;
                           end if;
                        end if;
                     when Page_Separator =>
                        Put(Destination, New_Paragraph_Sequence);
                        New_Line(Destination);
                        Put(Destination, New_Page_Sequence);
                        New_Line(Destination);
                     when others =>
                        Put_Line("   Bizarre separator at page " & Image(Page(Destination)) &
                                 ", line " & Image(Line(Destination)));
                        Put(Destination, Element(Lex.Image, Index => I));
                  end case;
               end loop;
            when Reserved_Word =>
               Put(Destination, Bold_Sequence);
               Put(Destination, To_String(Lex.Image));
               Put(Destination, Normal_Sequence);
            when Comment =>
               if Italicize_Comments then
                  Put(Destination, Italic_Sequence);
               end if;
               Put(Destination, Double(The_Char => '\', In_String => To_String(Lex.Image)));
               if Italicize_Comments then
                  Put(Destination, Normal_Sequence);
               end if;
            when Lexical_Error =>
               Put_Line("   Lexical error at page " & Image(Page(Destination)) &
                        ", line " & Image(Line(Destination)));
               Put(Destination, Double(The_Char => '\', In_String => To_String(Lex.Image)));
            when End_Of_Source =>
               exit;
            when others =>
               Put(Destination, Double(The_Char => '\', In_String => To_String(Lex.Image)));
         end case;
      end loop;
      Put(Destination, Document_End_Sequence);
      Close(Destination);
      N_Files_Processed := N_Files_Processed + 1;
   end RTF_Ada_File;

   procedure RTF_Ada_Files is new File_Name_Enumeration(RTF_Ada_File);


   function Image is new Number_Images.Integer_Image(Natural);


begin
   RTF_Ada_Files(Prompt => "Input File(s) : ");
   New_Line;
   Put_Line(Image(N_Files_Processed) & " file(s) processed");
end RTF_Ada_File;
