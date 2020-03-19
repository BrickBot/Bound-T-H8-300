with The_Ada_Lexical_Analyzer,
     Character_Handler,
     String_Case_Conversions,
     Varying_Text,
     Number_Images,
     Text_IO,
     Text_IO_Streams,
     Varying_Text_IO,
     VMS_File_Names,
     User_Interface;

use The_Ada_Lexical_Analyzer,
    Character_Handler,
    String_Case_Conversions,
    Varying_Text,
    Varying_Text_IO,
    Text_IO;

procedure Test_Ada_Lexical_Analyzer is
-----------------------------------

   Source       : Text_IO.File_Type;

   package Source_Stream is new Text_IO_Streams.Input_Stream(File => Source);


   Lex          : Lexical_Element;

   package Lexical_Element_Kind_Text_IO is new Text_IO.Enumeration_IO(Lexical_Element_Kind);
   use Lexical_Element_Kind_Text_IO;


   function Image is new Number_Images.Integer_Image(Text_IO.Positive_Count);
   function Image is new Number_Images.Integer_Image(Integer_Literal_Range);
   function Image is new Number_Images.Float_Image(Real_Literal_Digits);

begin
   if User_Interface.Yes_No_Answer("Analyze a source file ? ") then
      declare

         package The_Analyzer is
            new The_Ada_Lexical_Analyzer.Analyzer
                   (Get                        => Source_Stream.Get,
                    Include_Comments           => User_Interface.Yes_No_Answer("Include Comments ? "),
                    Include_Separators         => User_Interface.Yes_No_Answer("Include Separators ? "),
                    Allow_Alternate_Characters => User_Interface.Yes_No_Answer("Allow Alternate Characters ? "));

      begin
         Open(Source,
              Name => User_Interface.String_Answer(Prompt => "Source File Name : "),
              Mode => In_File);
         loop
            The_Analyzer.Get_Next_Lexical_Element(Lex);
            Put(Lex.Kind, Width => Lexical_Element_Kind'Width);
            Put(' ');
            if Lex.Kind = Separators then
               for I in 1..Length(Lex.Image) loop
                  case Char(Lex.Image, I) is
                     when ' ' =>
                        Put("<SP>");
                     when ASCII.HT =>
                        Put("<HT>");
                     when ASCII.VT =>
                        Put("<VT>");
                     when Line_Separator =>
                        Put("<CR>");
                     when Page_Separator =>
                        Put("<FF>");
                     when others =>
                        Put("<BIZARRE>");
                  end case;
               end loop;
            else
               Put('"' & Lex.Image & '"');
               Set_Col(50);
               case Lex.Kind is
                  when Delimiter =>
                     Put(Delimiter_Value'Image(Lex.The_Delimiter));
                  when Identifier =>
                     Put(Text(Lex.The_Identifier));
                  when Reserved_Word =>
                     Put(Text(Lex.The_Reserved_Word));
                  when Integer_Literal =>
                     begin
                        Put(Image(Lex.The_Integer_Literal.Value));
                     exception
                        when Constraint_Error =>
                           Put("-- out of range --");
                     end;
                  when Real_Literal =>
                     begin
                        Put(Image(Lex.The_Real_Literal.Value, Exp => 3));
                     exception
                        when Constraint_Error =>
                           Put("-- out of range --");
                     end;
                  when Character_Literal =>
                     Put(Character_Literal_Value'Image(Lex.The_Character_Literal));
                  when String_Literal =>
                     Put('"' & To_String(Lex.The_String_Literal) & '"');
                  when Comment =>
                     Put('"' & To_String(Lex.The_Comment) & '"');
                  when others =>
                     null;
               end case;
            end if;
            New_Line;
            exit when Lex.Kind = End_Of_Source;
         end loop;
         for I in 1..3 loop
            begin
               The_Analyzer.Get_Next_Lexical_Element(Lex);
               Put_Line("SOURCE_AT_END should have been raised");
            exception
               when Source_At_End =>
                  null;
            end;
         end loop;
      end;
   end if;
   ---
   if User_Interface.Yes_No_Answer("Copy a source file ? ") then
      declare

         package The_Analyzer is
            new The_Ada_Lexical_Analyzer.Analyzer
                   (Get                        => Source_Stream.Get,
                    Include_Separators         => True,
                    Allow_Alternate_Characters => True);

         type Word_Case is (Unchanged, Upper_Case, Lower_Case, Initials_Upper_Case);

         Source_Name          : constant String := User_Interface.String_Answer("   From => ");
         Destination_Name     : constant String := User_Interface.String_Answer("   To   => ");

         Destination          : Text_IO.File_Type;

         Identifier_Case,
         Reserved_Word_Case   : Word_Case;
         Number_Case          : Word_Case range Unchanged..Lower_Case;


         function Case_Answer (Prompt : String) return Word_Case is
         begin
            loop
               begin
                  return Word_Case'Value(User_Interface.String_Answer(Prompt));
               exception
                  when Constraint_Error =>
                     null;
               end;
            end loop;
         end Case_Answer;


         function Recased (Of_Image : String; Casing : Word_Case) return String is
         begin
            case Casing is
               when Unchanged =>
                  return Of_Image;
               when Upper_Case =>
                  return Upper_Case(Of_Image);
               when Lower_Case =>
                  return Lower_Case(Of_Image);
               when Initials_Upper_Case =>
                  declare

                     Result   : String(Of_Image'Range);
                     Upperize : Boolean := True;

                  begin
                     for I in Result'Range loop
                        if Upperize then
                           Result(I) := Upper_Case(Of_Image(I));
                        else
                           Result(I) := Lower_Case(Of_Image(I));
                        end if;
                        Upperize := Of_Image(I) = '_';
                     end loop;
                     return Result;
                  end;
            end case;
         end Recased;

      begin
         Put_Line("   -- Valid case values : Unchanged, Upper_Case, Lower_Case, Initials_Upper_Case");
         Identifier_Case    := Case_Answer("   Identifier case    : ");
         Reserved_Word_Case := Case_Answer("   Reserved word case : ");
         Number_Case        := Case_Answer("   Number case        : ");
         Open(Source, Name => Source_Name, Mode => In_File);
         Create(Destination, Name => Destination_Name);
         loop
            The_Analyzer.Get_Next_Lexical_Element(Lex);
            case Lex.Kind is
               when Separators =>
                  for I in 1..Length(Lex.Image) loop
                     case Char(Lex.Image, Position => I) is
                        when ' ' | ASCII.HT | ASCII.VT =>
                           Put(Destination, Char(Lex.Image, Position => I));
                        when Line_Separator =>
                           New_Line(Destination);
                        when Page_Separator =>
                           New_Page(Destination);
                        when others =>
                           Put_Line("   Bizarre separator at page " & Image(Page(Destination)) &
                                    ", line " & Image(Line(Destination)) &
                                    ", column " & Image(Col(Destination)));
                           Put(Destination, Char(Lex.Image, Position => I));
                     end case;
                  end loop;
               when Identifier =>
                  Put(Destination, Recased(To_String(Lex.Image), Casing => Identifier_Case));
               when Reserved_Word =>
                  Put(Destination, Recased(To_String(Lex.Image), Casing => Reserved_Word_Case));
               when Integer_Literal | Real_Literal =>
                  Put(Destination, Recased(To_String(Lex.Image), Casing => Number_Case));
               when Lexical_Error =>
                  Put_Line("   Lexical error at line " & Image(Line(Destination)));
                  Put(Destination, Lex.Image);
               when End_Of_Source =>
                  exit;
               when others =>
                  Put(Destination, Lex.Image);
            end case;
         end loop;
         Close(Destination);
      end;
   end if;
   ---
   if User_Interface.Yes_No_Answer("Check files for lexical errors ? ") then
      declare

         package The_Analyzer is
            new The_Ada_Lexical_Analyzer.Analyzer
                   (Get                        => Source_Stream.Get,
                    Include_Separators         => True,
                    Allow_Alternate_Characters => User_Interface.Yes_No_Answer("Allow Alternate Characters ? "));


         procedure Check_File_For_Lexical_Errors (File_Name : in String) is

            Error_Found  : Boolean := False;

            Current_Line : Text_IO.Positive_Count := 1;

         begin
            Open(Source, Name => File_Name, Mode => In_File);
            loop
               The_Analyzer.Get_Next_Lexical_Element(Lex);
               case Lex.Kind is
                  when Lexical_Error =>
                     Error_Found := True;
                     Put_Line("Lexical error at line " & Image(Current_Line) &
                              " in " & File_Name);
                  when Separators =>
                     for I in 1..Length(Lex.Image) loop
                        if Char(Lex.Image, I) = Line_Separator or Char(Lex.Image, I) = Page_Separator then
                           Current_Line := Current_Line + 1;
                        end if;
                     end loop;
                  when End_Of_Source =>
                     exit;
                  when others =>
                     null;
               end case;
            end loop;
         end Check_File_For_Lexical_Errors;

         procedure Check_Files_For_Lexical_Errors is
            new VMS_File_Names.Enumerate_Matching_Files(Check_File_For_Lexical_Errors);

      begin
         Check_Files_For_Lexical_Errors(File_Name => User_Interface.String_Answer("Source File(s) : "));
      end;
   end if;
   ---
   if User_Interface.Yes_No_Answer("Check files for letter case ? ") then
      declare

         package The_Analyzer is
            new The_Ada_Lexical_Analyzer.Analyzer
                   (Get                => Source_Stream.Get,
                    Include_Separators => True);


         procedure Check_File_For_Letter_Case (File_Name : in String) is

            Error_Found  : Boolean := False;

            Current_Line : Text_IO.Positive_Count := 1;

         begin
            Open(Source, Name => File_Name, Mode => In_File);
            loop
               The_Analyzer.Get_Next_Lexical_Element(Lex);
               case Lex.Kind is
                  when Separators =>
                     for I in 1..Length(Lex.Image) loop
                        if Char(Lex.Image, I) = Line_Separator or Char(Lex.Image, I) = Page_Separator then
                           Current_Line := Current_Line + 1;
                        end if;
                     end loop;
                  when Identifier =>
                     for I in 1..Length(Lex.Image) loop
                        if Is_Letter(Char(Lex.Image, I)) and then not Is_Upper_Case(Char(Lex.Image, I)) then
                           Put_Line("Identifier case error at line " & Image(Current_Line) & " in " & File_Name);
                           exit;
                        end if;
                     end loop;
                  when Reserved_Word =>
                     for I in 1..Length(Lex.Image) loop
                        if Is_Letter(Char(Lex.Image, I)) and then not Is_Lower_Case(Char(Lex.Image, I)) then
                           Put_Line("Reserved word case error at line " & Image(Current_Line) &
                                    " in " & File_Name);
                           exit;
                        end if;
                     end loop;
                  when Integer_Literal | Real_Literal =>
                     for I in 1..Length(Lex.Image) loop
                        if Is_Letter(Char(Lex.Image, I)) and then not Is_Upper_Case(Char(Lex.Image, I)) then
                           Put_Line("Numeric literal case error at line " & Image(Current_Line) &
                                    " in " & File_Name);
                           exit;
                        end if;
                     end loop;
                  when Lexical_Error =>
                     Error_Found := True;
                     Put_Line("Lexical error at line " & Image(Current_Line) & " in " & File_Name);
                  when others =>
                     null;
               end case;
               exit when Lex.Kind = End_Of_Source;
            end loop;
         end Check_File_For_Letter_Case;

         procedure Check_Files_For_Letter_Case is
            new VMS_File_Names.Enumerate_Matching_Files(Check_File_For_Letter_Case);

      begin
         Check_Files_For_Letter_Case(File_Name => User_Interface.String_Answer("Source File(s) : "));
      end;
   end if;
end Test_Ada_Lexical_Analyzer;
